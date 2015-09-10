package com.sungardas.snapdirector.service.impl;


import com.amazonaws.AmazonClientException;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.GetObjectRequest;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.amazonaws.services.s3.model.S3Object;
import com.amazonaws.services.s3.model.S3ObjectInputStream;
import com.sungardas.snapdirector.exception.ConfigurationException;
import com.sungardas.snapdirector.exception.SDFSException;
import com.sungardas.snapdirector.service.SDFSStateService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.context.support.XmlWebApplicationContext;

import java.io.*;
import java.net.URI;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;

@Service
public class SDFSStateServiceImpl implements SDFSStateService {
    private static final org.apache.log4j.Logger LOG = org.apache.log4j.LogManager.getLogger(SDFSStateServiceImpl.class);

    @Value("${amazon.aws.accesskey:}")
    private String amazonAWSAccessKey;
    @Value("${amazon.aws.secretkey}")
    private String amazonAWSSecretKey;

    @Autowired
    private AmazonS3 amazonS3;

    @Value("${amazon.s3.bucket}")
    private String s3Bucket;

    @Value("${sdfs.volume.metadata.path}")
    private String volumeMetadataPath;

    @Value("${sdfs.volume.config.path}")
    private String volumeConfigPath;

    @Autowired
    private XmlWebApplicationContext applicationContext;

    private static final String SDFS_STATE_BACKUP_PATH = "/var/sdfs/sdfsstate.zip";
    private static final String KEY_NAME = "sdfsstate.zip";
    private static final String SDFS_STATE_DESTINATION = "/";

    @Override
    public void backupState() throws AmazonClientException{
        String[] paths = {volumeMetadataPath,volumeConfigPath};
        try {
            ZipUtils.zip(SDFS_STATE_BACKUP_PATH, paths);
        } catch (IOException e) {
            String errMsg = String.format("Cant zip to archive %s", SDFS_STATE_BACKUP_PATH);
            LOG.error(errMsg);
           throw new SDFSException(errMsg, e);
        }
        uploadToS3(KEY_NAME, new File(SDFS_STATE_BACKUP_PATH));
    }

    @Override
    public void restoreState() throws AmazonClientException{
        try {
            downloadFromS3(KEY_NAME,SDFS_STATE_BACKUP_PATH);
            ZipUtils.unzip(SDFS_STATE_BACKUP_PATH, SDFS_STATE_DESTINATION);

        } catch (IOException e) {
            String errMsg = String.format("Cant restore sdfs state from archive %s", SDFS_STATE_BACKUP_PATH);
            LOG.error(errMsg);
            throw new SDFSException(errMsg, e);
        }
    }

    @Override
    public boolean containsSdfsMetadata(String sBucket) {
        return false;
    }

    @Override
    public void createSDFS(String size, String bucketName) {
        try {
            File file = applicationContext.getResource("classpath:sdfs1.sh").getFile();
            file.setExecutable(true);
            String pathToExec = file.getAbsolutePath();
            String[] parameters = {pathToExec, amazonAWSAccessKey, amazonAWSSecretKey, size, bucketName};
            Process p = Runtime.getRuntime().exec(parameters);
            p.waitFor();
            print(p);
            switch (p.exitValue()) {
                case 0:
                    LOG.info("SDFS mounted");
                    break;
                case 1:
                    LOG.info("SDFS unmounted");
                    p = Runtime.getRuntime().exec(parameters);
                    p.waitFor();
                    print(p);
                    if (p.exitValue() != 0) {
                        throw new ConfigurationException("Error creating sdfs");
                    }
                    LOG.info("SDFS mounted");
                    break;
                default:
                    print(p);
                    throw new ConfigurationException("Error creating sdfs");
            }
        } catch (IOException e) {
            LOG.error(e);
        } catch (InterruptedException e) {
            LOG.error(e);
        }
    }

    private void print(Process p) throws IOException {
        String line;
        BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
        while ((line = input.readLine()) != null) {
            System.out.println(line);
        }
        input = new BufferedReader(new InputStreamReader(p.getErrorStream()));
        while ((line = input.readLine()) != null) {
            System.out.println(line);
        }
    }


    private void uploadToS3(String keyName, File sdfsBackupFile) {
        PutObjectRequest putObjectRequest = new PutObjectRequest(s3Bucket, keyName, sdfsBackupFile);
        amazonS3.putObject(putObjectRequest);
    }

    private File downloadFromS3(String keyName, String sdfsBackupFile) throws IOException {
        GetObjectRequest getObjectRequest = new GetObjectRequest(s3Bucket, keyName);
        S3Object s3object = amazonS3.getObject(getObjectRequest);

        File result = new File(sdfsBackupFile);
        BufferedOutputStream bout = new BufferedOutputStream(new FileOutputStream(result));

        int count;
        byte[] buffer = new byte[2048];
        S3ObjectInputStream s3in = s3object.getObjectContent();
        while ((count = s3in.read(buffer)) != -1) {
            bout.write(buffer, 0, count);
        }
        bout.flush();
        bout.close();
        return result;
    }
}


class ZipUtils {

    private static final Logger LOG = LogManager.getLogger(ZipUtils.class);

    private static FileSystem createZipFileSystem(String zipFilename,
                                                  boolean create)
            throws IOException {
        // convert the filename to a URI
        final Path path = Paths.get(zipFilename);
        final URI uri = URI.create("jar:file:" + path.toUri().getPath().replaceAll(" ", "%2520"));

        final Map<String, String> env = new HashMap<>();
        if (create) {
            env.put("create", "true");
        }
        return FileSystems.newFileSystem(uri, env);
    }

    public static void unzip(String zipFilename, String destDirname)
            throws IOException{

        final Path destDir = Paths.get(destDirname);
        //if the destination doesn't exist, create it
        if(Files.notExists(destDir)){
            LOG.info(destDir + " does not exist. Creating...");
            Files.createDirectories(destDir);
        }

        try (FileSystem zipFileSystem = createZipFileSystem(zipFilename, false)){
            final Path root = zipFileSystem.getPath("/");

            //walk the zip file tree and copy files to the destination
            Files.walkFileTree(root, new SimpleFileVisitor<Path>(){
                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs) throws IOException {
                    final Path destFile = Paths.get(destDir.toString(),
                            file.toString());
                    LOG.info("Extracting file %s to %s\n", file, destFile);
                    Files.copy(file, destFile, StandardCopyOption.REPLACE_EXISTING);
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(Path dir,
                                                         BasicFileAttributes attrs) throws IOException {
                    final Path dirToCreate = Paths.get(destDir.toString(),
                            dir.toString());
                    if(Files.notExists(dirToCreate)){
                        LOG.info("Creating directory %s\n", dirToCreate);
                        Files.createDirectory(dirToCreate);
                    }
                    return FileVisitResult.CONTINUE;
                }
            });
        }
    }

    public static void zip(String zipFilename, String... filenames)
            throws IOException {

        try (FileSystem zipFileSystem = createZipFileSystem(zipFilename, true)) {
            final Path root = zipFileSystem.getPath("/");

            //iterate over the files we need to add
            for (String filename : filenames) {
                final Path src = Paths.get(filename);

                //add a file to the zip file system
                if(!Files.isDirectory(src)){
                    final Path dest = zipFileSystem.getPath(root.toString(),
                            src.toString());
                    final Path parent = dest.getParent();
                    if(Files.notExists(parent)){
                        LOG.info("Creating directory %s\n", parent);
                        Files.createDirectories(parent);
                    }
                    Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING);
                }
                else{
                    //for directories, walk the file tree
                    Files.walkFileTree(src, new SimpleFileVisitor<Path>(){
                        @Override
                        public FileVisitResult visitFile(Path file,
                                                         BasicFileAttributes attrs) throws IOException {
                            final Path dest = zipFileSystem.getPath(root.toString(),
                                    file.toString());
                            Files.copy(file, dest, StandardCopyOption.REPLACE_EXISTING);
                            return FileVisitResult.CONTINUE;
                        }

                        @Override
                        public FileVisitResult preVisitDirectory(Path dir,
                                                                 BasicFileAttributes attrs) throws IOException {
                            final Path dirToCreate = zipFileSystem.getPath(root.toString(),
                                    dir.toString());
                            if(Files.notExists(dirToCreate)){
                                LOG.info("Creating directory %s\n", dirToCreate);
                                Files.createDirectories(dirToCreate);
                            }
                            return FileVisitResult.CONTINUE;
                        }
                    });
                }
            }
        }
    }

    public static void list(String zipFilename) throws IOException{

        LOG.info("Listing Archive:  %s\n", zipFilename);

        //create the file system
        try (FileSystem zipFileSystem = createZipFileSystem(zipFilename, false)) {

            final Path root = zipFileSystem.getPath("/");

            //walk the file tree and print out the directory and filenames
            Files.walkFileTree(root, new SimpleFileVisitor<Path>(){
                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs) throws IOException {
                    print(file);
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(Path dir,
                                                         BasicFileAttributes attrs) throws IOException {
                    print(dir);
                    return FileVisitResult.CONTINUE;
                }

                private void print(Path file) throws IOException{
                    final DateFormat df = new SimpleDateFormat("MM/dd/yyyy-HH:mm:ss");
                    final String modTime= df.format(new Date(
                            Files.getLastModifiedTime(file).toMillis()));
                    LOG.info("%d  %s  %s\n",
                            Files.size(file),
                            modTime,
                            file);
                }
            });
        }
    }
}
package com.sungardas.enhancedsnapshots.service.impl;

import com.amazonaws.AmazonClientException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.*;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.BackupEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.BackupState;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.BackupRepository;
import com.sungardas.enhancedsnapshots.exception.ConfigurationException;
import com.sungardas.enhancedsnapshots.exception.SDFSException;
import com.sungardas.enhancedsnapshots.service.SDFSStateService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;
import org.springframework.web.context.support.XmlWebApplicationContext;

import java.io.*;
import java.net.URI;
import java.nio.file.FileSystem;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@Profile("prod")
public class SDFSStateServiceImpl implements SDFSStateService {
    private static final org.apache.log4j.Logger LOG = org.apache.log4j.LogManager.getLogger(SDFSStateServiceImpl.class);

    @Autowired
    private AWSCredentials credentials;

    @Autowired
    private AmazonS3 amazonS3;

    @Autowired
    private BackupRepository backupRepository;

    @Value("${amazon.s3.bucket}")
    private String s3Bucket;

    @Value("${sdfs.volume.metadata.path}")
    private String volumeMetadataPath;

    @Value("${sdfs.volume.config.path}")
    private String volumeConfigPath;

    @Value("${amazon.sdfs.size}")
    private String sdfsSize;

    @Value("${sungardas.worker.configuration}")
    private String instanceId;

    private static final String SDFS_MOUNT_POINT = "/mnt/awspool/";

    @Autowired
    private XmlWebApplicationContext applicationContext;

    public static final String SDFS_STATE_BACKUP_FILE_NAME = "sdfsstate";
    public static final String SDFS_STATE_BACKUP_FILE_EXT = ".zip";
    private static final String KEY_NAME = "sdfsstate.zip";
    private static final String SDFS_STATE_DESTINATION = "/";

    private static final int VOLUME_ID_INDEX = 0;
    private static final int TIME_INDEX = 1;
    private static final int TYPE_INDEX = 2;
    private static final int IOPS_INDEX = 3;

    private static final long BYTES_IN_GIB = 1073741824l;

    @Override
    public void backupState() throws AmazonClientException {
        shutdownSDFS(sdfsSize, s3Bucket);
        String[] paths = {volumeMetadataPath, volumeConfigPath};
        File tempFile = null;
        try {
            tempFile = ZipUtils.zip(paths);
        } catch (Throwable e) {
            startupSDFS(sdfsSize, s3Bucket);
            if (tempFile != null && tempFile.exists()) {
                tempFile.delete();
            }
            throw new SDFSException("Cant create system backup ", e);
        }
        uploadToS3(KEY_NAME, tempFile);
        startupSDFS(sdfsSize, s3Bucket);
        tempFile.delete();
    }

    @Override
    public void restoreState() throws AmazonClientException {
        shutdownSDFS(sdfsSize, s3Bucket);
        File file = null;
        try {
            file = Files.createTempFile(SDFS_STATE_BACKUP_FILE_NAME, SDFS_STATE_BACKUP_FILE_EXT).toFile();
            downloadFromS3(KEY_NAME, file);
            ZipUtils.unzip(file, SDFS_STATE_DESTINATION);
            file.delete();
            startupSDFS(sdfsSize, s3Bucket);
            //SDFS mount time
            Thread.sleep(15000);
            restoreBackups();
        } catch (Exception e) {
            if (file != null && file.exists()) {
                file.delete();
            }
            throw new SDFSException("Cant restore sdfs state", e);
        }
    }

    private void restoreBackups() {
        File[] files = new File(SDFS_MOUNT_POINT).listFiles();
        System.out.println("Found "+files.length+" files in system backup");
        for (File file : files) {
            BackupEntry entry = getBackupFromFile(file);
            if (entry != null) {
                backupRepository.save(entry);
            }
        }
    }

    private BackupEntry getBackupFromFile(File file) {
        String fileName = file.getName();
        String[] props = fileName.split("\\.");
        if (props.length != 5) {
            return null;
        } else {
            BackupEntry backupEntry = new BackupEntry();

            backupEntry.setFileName(fileName);
            backupEntry.setInstanceId(instanceId);
            backupEntry.setIops(props[IOPS_INDEX]);
            backupEntry.setSizeGiB(String.valueOf((int) (file.length() / BYTES_IN_GIB)));
            backupEntry.setTimeCreated(props[TIME_INDEX]);
            backupEntry.setVolumeType(props[TYPE_INDEX]);
            backupEntry.setState(BackupState.COMPLETED.getState());
            backupEntry.setVolumeId(props[VOLUME_ID_INDEX]);
            backupEntry.setSize(String.valueOf(file.length()));

            return backupEntry;
        }
    }

    @Override
    public boolean containsSdfsMetadata(String sBucket) {
        ListObjectsRequest request = new ListObjectsRequest()
                .withBucketName(sBucket).withPrefix("sdfsstate");
        return amazonS3.listObjects(request).getObjectSummaries().size() > 0;

    }

    @Override
    public Long getBackupTime() {
        ListObjectsRequest request = new ListObjectsRequest()
                .withBucketName(s3Bucket).withPrefix(KEY_NAME);
        List<S3ObjectSummary> list = amazonS3.listObjects(request).getObjectSummaries();
        if (list.size() > 0) {
            return list.get(0).getLastModified().getTime();
        } else {
            return null;
        }
    }

    @Override
    public void startupSDFS(String size, String bucketName) {
        try {
            File file = applicationContext.getResource("classpath:sdfs1.sh").getFile();
            file.setExecutable(true);
            String pathToExec = file.getAbsolutePath();
            LOG.info("getAWSAccessKeyId: "+credentials.getAWSAccessKeyId()+"; getAWSSecretKey: "+credentials.getAWSSecretKey());
            String[] parameters = {pathToExec, credentials.getAWSAccessKeyId(), credentials.getAWSSecretKey(), size, bucketName};
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
        } catch (Exception e) {
            LOG.error(e);
            throw new ConfigurationException("Error creating sdfs");
        }
    }

    @Override
    public void shutdownSDFS(String size, String bucketName) {
        try {
            File file = applicationContext.getResource("classpath:sdfs1.sh").getFile();
            file.setExecutable(true);
            String pathToExec = file.getAbsolutePath();
            String[] parameters = {pathToExec, credentials.getAWSAccessKeyId(), credentials.getAWSSecretKey(), size, bucketName};
            Process p = Runtime.getRuntime().exec(parameters);
            p.waitFor();
            print(p);
            switch (p.exitValue()) {
                case 0:
                    LOG.info("SDFS mounted");
                    p = Runtime.getRuntime().exec(parameters);
                    p.waitFor();
                    print(p);
                    if (p.exitValue() != 1) {
                        throw new ConfigurationException("Error unable to stop sdfs");
                    }
                    LOG.info("SDFS unmounted");
                    break;
                case 1:
                    LOG.info("SDFS unmounted");
                    break;
                default:
                    print(p);
                    throw new ConfigurationException("Error creating sdfs");
            }
        } catch (Exception e) {
            LOG.error(e);
            throw new ConfigurationException("Error creating sdfs");
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

    private void downloadFromS3(String keyName, File sdfsBackupFile) throws IOException {
        GetObjectRequest getObjectRequest = new GetObjectRequest(s3Bucket, keyName);
        S3Object s3object = amazonS3.getObject(getObjectRequest);

        BufferedOutputStream bout = new BufferedOutputStream(new FileOutputStream(sdfsBackupFile));

        int count;
        byte[] buffer = new byte[2048];
        S3ObjectInputStream s3in = s3object.getObjectContent();
        while ((count = s3in.read(buffer)) != -1) {
            bout.write(buffer, 0, count);
        }
        bout.flush();
        bout.close();
    }

    static class ZipUtils {

        private static final Logger LOG = LogManager.getLogger(ZipUtils.class);

        private static FileSystem createZipFileSystem(File file, boolean create) throws IOException {
            final URI uri = URI.create("jar:file:" + file.toURI().getPath().replaceAll(" ", "%2520"));

            final Map<String, String> env = new HashMap<>();
            if (create) {
                env.put("create", "true");
            }
            return FileSystems.newFileSystem(uri, env);
        }

        public static void unzip(File zipFile, String destDirname)
                throws IOException {

            final Path destDir = Paths.get(destDirname);
            //if the destination doesn't exist, create it
            if (Files.notExists(destDir)) {
                LOG.info(destDir + " does not exist. Creating...");
                Files.createDirectories(destDir);
            }

            try (FileSystem zipFileSystem = createZipFileSystem(zipFile, false)) {
                final Path root = zipFileSystem.getPath("/");

                //walk the zip file tree and copy files to the destination
                Files.walkFileTree(root, new SimpleFileVisitor<Path>() {
                    @Override
                    public FileVisitResult visitFile(Path file,
                                                     BasicFileAttributes attrs) throws IOException {
                        final Path destFile = Paths.get(destDir.toString(),
                                file.toString());
                        LOG.info("Extracting file {} to {}\n", file, destFile);
                        Files.copy(file, destFile, StandardCopyOption.REPLACE_EXISTING);
                        return FileVisitResult.CONTINUE;
                    }

                    @Override
                    public FileVisitResult preVisitDirectory(Path dir,
                                                             BasicFileAttributes attrs) throws IOException {
                        final Path dirToCreate = Paths.get(destDir.toString(),
                                dir.toString());
                        if (Files.notExists(dirToCreate)) {
                            LOG.info("Creating directory {}\n", dirToCreate);
                            Files.createDirectory(dirToCreate);
                        }
                        return FileVisitResult.CONTINUE;
                    }
                });
            }
        }

        public static File zip(String... filenames) throws IOException {
            File tempFile = Files.createTempFile(SDFS_STATE_BACKUP_FILE_NAME, SDFS_STATE_BACKUP_FILE_EXT).toFile();
            tempFile.delete();
            try (FileSystem zipFileSystem = createZipFileSystem(tempFile, true)) {
                final Path root = zipFileSystem.getPath("/");

                //iterate over the files we need to add
                for (String filename : filenames) {
                    final Path src = Paths.get(filename);

                    //add a file to the zip file system
                    if (!Files.isDirectory(src)) {
                        final Path dest = zipFileSystem.getPath(root.toString(),
                                src.toString());
                        final Path parent = dest.getParent();
                        if (Files.notExists(parent)) {
                            LOG.info("Creating directory {}", parent);
                            Files.createDirectories(parent);
                        }
                        Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING);
                    } else {
                        //for directories, walk the file tree
                        Files.walkFileTree(src, new SimpleFileVisitor<Path>() {
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
                                if (Files.notExists(dirToCreate)) {
                                    LOG.info("Creating directory {}\n", dirToCreate);
                                    Files.createDirectories(dirToCreate);
                                }
                                return FileVisitResult.CONTINUE;
                            }
                        });
                    }
                }
            }
            return tempFile;
        }
    }
}
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
import org.kamranzafar.jtar.TarEntry;
import org.kamranzafar.jtar.TarInputStream;
import org.kamranzafar.jtar.TarOutputStream;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.context.support.XmlWebApplicationContext;

import java.io.*;
import java.util.*;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

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

    private static final String SDFS_STATE_BACKUP_NAME = "volumemetadata.tar.gz";

    @Override
    public void backupState() throws AmazonClientException{
        String[] pathes = {volumeMetadataPath,volumeConfigPath};
        File fileToUpload = TarUtils.packToTar(SDFS_STATE_BACKUP_NAME, Arrays.asList(pathes));
        uploadToS3(SDFS_STATE_BACKUP_NAME, fileToUpload);
    }

    @Override
    public void restoreState() throws AmazonClientException{
        try {
            File tmpTarGz = downloadFromS3("tmp.tar.gz",SDFS_STATE_BACKUP_NAME);
            TarUtils.unpackFromTar(tmpTarGz);

        } catch (IOException e) {
            e.printStackTrace();
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


class TarUtils {

    public static final Logger LOG = LogManager.getLogger(TarUtils.class);

    public static File packToTar(String archiveName, String pathToPack) {
        LinkedList<String>pathes = new LinkedList<>();
        pathes.add(pathToPack);
        return packToTar(archiveName, pathToPack);
    }

    public static File packToTar(String archiveName, Collection<String> pathsToStore) {
        try {
            LOG.info("\nCreating archive: {}", archiveName);
            File result = new File(archiveName);
            FileOutputStream archiveDestination = new FileOutputStream(result);
            TarOutputStream tarOut = new TarOutputStream(new BufferedOutputStream(new GZIPOutputStream(
                    archiveDestination)));

            File f;
            for (String filepath : pathsToStore) {
                LOG.info("Storing location: {}", filepath);
                f = new File(filepath);
                if (!f.exists())
                    continue;
                if (f.isDirectory()) {
                    pachFolderToTar(tarOut, f);
                    continue;
                }
                putFileToTar(tarOut, f);
            }

            tarOut.close();
            return result;

        } catch (FileNotFoundException e) {
            LOG.error("Cant create archive file: " + archiveName);
            LOG.error("Error Message:    " + e.getMessage());
            throw new RuntimeException(e);
        } catch (IOException e) {
            LOG.error(e);
            throw new SDFSException(e);
        }
    }


    private static final void pachFolderToTar(TarOutputStream tarOut, File folder) throws IOException {
        File[] files = folder.listFiles();

        for (File f : files) {
            if (f.isDirectory()) {
                pachFolderToTar(tarOut, f);
                continue;
            }
            putFileToTar(tarOut, f);
        }

    }


    private static void putFileToTar(TarOutputStream tarOut, File fileToPut) throws IOException {
        tarOut.putNextEntry(new TarEntry(fileToPut, fileToPut.getAbsolutePath()));

        BufferedInputStream origin = new BufferedInputStream(new FileInputStream(fileToPut));
        int count;
        byte buffer[] = new byte[2048];
        while ((count = origin.read(buffer)) != -1) {
            tarOut.write(buffer, 0, count);
        }
        tarOut.flush();
        origin.close();
    }


    public static void unpackFromTar(File archiveName) {
        try {
            LOG.info("\nExtracting archive: {}", archiveName.getName());
            FileInputStream archiveSource = new FileInputStream(archiveName);
            TarInputStream tarIn = new TarInputStream(new BufferedInputStream(new GZIPInputStream(archiveSource)));

            TarEntry entry;

            while ((entry = tarIn.getNextEntry()) != null) {
                int count;
                byte buffer[] = new byte[2048];
                String absolutePath = entry.getName();
                createDirs(absolutePath);

                BufferedOutputStream destination = new BufferedOutputStream(new FileOutputStream(absolutePath));
                while ((count = tarIn.read(buffer)) != -1) {
                    destination.write(buffer, 0, count);
                }
                destination.flush();
                destination.close();
            }

            archiveSource.close();
            LOG.info("Archive extracted: " + archiveName);
        } catch (FileNotFoundException e) {
            LOG.error("Cant get access to archive file: " + archiveName);
            LOG.error("Error Message:    " + e.getMessage());
            throw new RuntimeException(e);
        } catch (IOException e) {
            e.printStackTrace();
        }

    }


    private static void createDirs(String absolutePath) {
        int fileNamePos = absolutePath.lastIndexOf('\\') != -1 ? absolutePath.lastIndexOf('\\') : absolutePath
                .lastIndexOf('/');
        File pathToCreate = new File(absolutePath.substring(0, fileNamePos));
        if (pathToCreate.mkdirs()) {
            LOG.info("Created folder: {}", pathToCreate);
        }
    }
}
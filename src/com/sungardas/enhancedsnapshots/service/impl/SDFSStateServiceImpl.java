package com.sungardas.enhancedsnapshots.service.impl;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import com.amazonaws.regions.Regions;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.GetObjectRequest;
import com.amazonaws.services.s3.model.ListObjectsRequest;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.amazonaws.services.s3.model.S3Object;
import com.amazonaws.services.s3.model.S3ObjectInputStream;
import com.amazonaws.services.s3.model.S3ObjectSummary;
import com.sungardas.enhancedsnapshots.components.ConfigurationMediator;
import com.sungardas.enhancedsnapshots.exception.ConfigurationException;
import com.sungardas.enhancedsnapshots.exception.SDFSException;
import com.sungardas.enhancedsnapshots.service.NotificationService;
import com.sungardas.enhancedsnapshots.service.SDFSStateService;

import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.core.io.ResourceLoader;
import org.springframework.stereotype.Service;


@Service
@Profile("prod")
public class SDFSStateServiceImpl implements SDFSStateService {

    private static final Logger LOG = LogManager.getLogger(SDFSStateServiceImpl.class);

    private boolean reconfigurationInProgressFlag = false;

    private static final String MOUNT_CMD = "--mount";
    private static final String UNMOUNT_CMD = "--unmount";
    private static final String GET_SATE_CMD = "--state";
    private static final String CONFIGURE_CMD = "--configure";
    private static final String EXPAND_VOLUME_CMD = "--expandvolume";

    @Value("${enhancedsnapshots.default.sdfs.mount.time}")
    private int sdfsMountTime;
    @Value("${enhancedsnapshots.sdfs.script.path}")
    private String sdfsScript;


    @Autowired
    private ResourceLoader resourceLoader;

    @Autowired
    private AmazonS3 amazonS3;

    @Autowired
    private NotificationService notificationService;

    @Autowired
    private ConfigurationMediator configurationMediator;

    //TODO: maybe we can avoid stopping and starting sdfs while backup if we use sdfscli --cloud-sync-fs command
    //TODO: in sdfs 3.1.9 this command does not work but according to documentation it should be supported
    @Override
    public void backupState(String taskId) {
        if (taskId != null) {
            notificationService.notifyAboutTaskProgress(taskId, "Stopping SDFS...", 20);
        }
        stopSDFS();
        String[] paths = {configurationMediator.getSdfsConfigPath()};
        File tempFile = null;
        try {
            if (taskId != null) {
                notificationService.notifyAboutTaskProgress(taskId, "Compressing SDFS files...", 40);
            }
            tempFile = ZipUtils.zip(FilenameUtils.removeExtension(configurationMediator.getSdfsBackupFileName()),
                    getFileExtension(configurationMediator.getSdfsBackupFileName()), paths);
        } catch (Throwable e) {
            startSDFS();
            if (tempFile != null && tempFile.exists()) {
                tempFile.delete();
            }
            throw new SDFSException("Cant create system backup ", e);
        }
        if (taskId != null) {
            notificationService.notifyAboutTaskProgress(taskId, "Uploading SDFS files to S3...", 60);
        }
        uploadToS3(configurationMediator.getSdfsBackupFileName(), tempFile);
        if (taskId != null) {
            notificationService.notifyAboutTaskProgress(taskId, "Starting SDFS...", 80);
        }
        startSDFS();
        tempFile.delete();
    }

    @Override
    public void restoreSDFS() {
        restoreSDFS(configurationMediator.getSdfsBackupFileName());
    }

    @Override
    public void restoreSDFS(String fromArchive) {
        File file = null;
        try {
            removeSdfsConfFile();
            // copy sdfs config file from S3
            file = Files.createTempFile(FilenameUtils.removeExtension(fromArchive),
                    getFileExtension(fromArchive)).toFile();
            downloadFromS3(fromArchive, file);
            ZipUtils.unzip(file, new File(configurationMediator.getSdfsConfigPath()).getParentFile().getAbsolutePath());
            file.delete();
            startSDFS(true);
            //SDFS mount time
            TimeUnit.SECONDS.sleep(sdfsMountTime);
            LOG.info("SDFS state restored.");
        } catch (Exception e) {
            if (file != null && file.exists()) {
                file.delete();
            }
            throw new SDFSException("Can't restore sdfs state", e);
        }
    }


    @Override
    public boolean containsSdfsMetadata(String sBucket) {
        ListObjectsRequest request = new ListObjectsRequest()
                .withBucketName(sBucket).withPrefix(configurationMediator.getSdfsBackupFileName());
        return amazonS3.listObjects(request).getObjectSummaries().size() > 0;

    }

    @Override
    public Long getBackupTime() {
        ListObjectsRequest request = new ListObjectsRequest()
                .withBucketName(configurationMediator.getS3Bucket()).withPrefix(configurationMediator.getSdfsBackupFileName());
        List<S3ObjectSummary> list = amazonS3.listObjects(request).getObjectSummaries();
        if (list.size() > 0) {
            return list.get(0).getLastModified().getTime();
        } else {
            return null;
        }
    }

    @Override
    public synchronized void reconfigureAndRestartSDFS() {
        try {
            reconfigurationInProgressFlag = true;
            stopSDFS();
            removeSdfsConfFile();
            configureSDFS();
            startSDFS(false);
        } catch (Exception e) {
            LOG.error("Failed to reconfigure and restart SDFS", e);
            throw new ConfigurationException("Failed to reconfigure and restart SDFS");
        } finally {
            reconfigurationInProgressFlag = false;
        }
    }

    private void startSDFS(Boolean restore) {
        try {
            if (sdfsIsRunnig()){
                LOG.info("SDFS is already running");
                return;
            }
            if (!new File(configurationMediator.getSdfsConfigPath()).exists()) {
                configureSDFS();
            }
            String[] parameters = {getSdfsScriptFile(sdfsScript).getAbsolutePath(), MOUNT_CMD, restore.toString()};
            Process p = executeScript(parameters);
            switch (p.exitValue()) {
                case 0:
                    LOG.info("SDFS is running");
                    break;
                default:
                    throw new ConfigurationException("Failed to start SDFS");
            }
        } catch (Exception e) {
            LOG.error(e);
            throw new ConfigurationException("Failed to start SDFS");
        }
    }

    @Override
    public void startSDFS (){
        startSDFS(false);
    }


    private void configureSDFS() throws IOException, InterruptedException {
        String[] parameters = {getSdfsScriptFile(sdfsScript).getAbsolutePath(), CONFIGURE_CMD, configurationMediator.getSdfsVolumeSize(), configurationMediator.getS3Bucket(),
                getBucketLocation(configurationMediator.getS3Bucket()), configurationMediator.getSdfsLocalCacheSize()};
        Process p = executeScript(parameters);
        switch (p.exitValue()) {
            case 0:
                LOG.info("SDFS is configured");
                break;
            default:
                throw new ConfigurationException("Failed to configure SDFS");
        }
    }

    @Override
    public void stopSDFS() {
        try {
            if (!sdfsIsRunnig()){
                LOG.info("SDFS is already stopped");
                return;
            }
            String[] parameters = {getSdfsScriptFile(sdfsScript).getAbsolutePath(), UNMOUNT_CMD};
            Process p = executeScript(parameters);
            switch (p.exitValue()) {
                case 0:
                    LOG.info("SDFS is currently stopped");
                    break;
                default:
                    throw new ConfigurationException("Failed to stop SDFS");
            }
        } catch (Exception e) {
            LOG.error(e);
            throw new ConfigurationException("Failed to stop SDFS");
        }
    }

    private boolean sdfsIsRunnig() {
        try {
            String[] parameters = {getSdfsScriptFile(sdfsScript).getAbsolutePath(), GET_SATE_CMD};
            Process p = executeScript(parameters);
            switch (p.exitValue()) {
                case 0:
                    LOG.debug("SDFS is currently running");
                    return true;
                case 1:
                    LOG.debug("SDFS is currently stopped");
                    return false;
                default:
                    throw new ConfigurationException("Failed to stop SDFS");
            }
        } catch (Exception e) {
            LOG.error(e);
            throw new ConfigurationException("Failed to determine SDFS state");
        }
    }

    public boolean sdfsIsAvailable() {
        try {
            if (reconfigurationInProgressFlag) {
                LOG.debug("SDFS is unavailable. Reconfiguration is in progress ... ");
                return false;
            }
            return sdfsIsRunnig();
        } catch (Exception e) {
            LOG.error(e);
            throw new ConfigurationException("Failed to determine SDFS state");
        }
    }

    @Override
    public void expandSdfsVolume(String newVolumeSize) {
        String[] parameters;
        try {
            parameters = new String[]{getSdfsScriptFile(sdfsScript).getAbsolutePath(), EXPAND_VOLUME_CMD, configurationMediator.getSdfsMountPoint(), newVolumeSize};
            Process p = executeScript(parameters);
            switch (p.exitValue()) {
                case 0:
                    LOG.debug("SDFS volume was expanded successfully");
                    break;
                case 1:
                    LOG.debug("Failed to expand SDFS volume");
                default:
                    throw new ConfigurationException("Failed to stop SDFS");
            }
        } catch (Exception e) {
            LOG.error(e);
            throw new ConfigurationException("Failed to expand SDFS volume");
        }
    }

    private void removeSdfsConfFile() {
        File sdfsConf = new File(configurationMediator.getSdfsConfigPath());
        if (sdfsConf.exists()) {
            sdfsConf.delete();
            LOG.info("SDFS conf file was successfully removed.");
        }
    }

    private Process executeScript(String[] parameters) throws IOException, InterruptedException {
        LOG.info("Executing script: {}", Arrays.toString(parameters));
        Process p = Runtime.getRuntime().exec(parameters);
        p.waitFor();
        print(p);
        return p;
    }

    private String getBucketLocation(String bucket) {
        String location;
        if (amazonS3.doesBucketExist(bucket)) {
            location = amazonS3.getBucketLocation(bucket);
        }
        else {
            location = Regions.getCurrentRegion().getName();
        }

        return location;
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
        PutObjectRequest putObjectRequest = new PutObjectRequest(configurationMediator.getS3Bucket(), keyName, sdfsBackupFile);
        amazonS3.putObject(putObjectRequest);
    }

    private void downloadFromS3(String keyName, File sdfsBackupFile) throws IOException {
        GetObjectRequest getObjectRequest = new GetObjectRequest(configurationMediator.getS3Bucket(), keyName);
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

    private File getSdfsScriptFile(String scriptName) throws IOException {
        File sdfsScript = resourceLoader.getResource(scriptName).getFile();
        sdfsScript.setExecutable(true);
        return sdfsScript;
    }

    private String getFileExtension(String fileName) {
        return "." + FilenameUtils.getExtension(fileName);
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

        public static File zip(String tempFileName, String tempFileExt, String... filenames) throws IOException {
            File tempFile = Files.createTempFile(tempFileName, tempFileExt).toFile();
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
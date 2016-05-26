package com.sungardas.enhancedsnapshots.service.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import javax.annotation.PostConstruct;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapperConfig;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBScanExpression;
import com.amazonaws.services.ec2.model.VolumeType;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.Bucket;
import com.amazonaws.services.s3.model.GetObjectRequest;
import com.amazonaws.services.s3.model.ListObjectsRequest;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.amazonaws.services.s3.model.S3Object;
import com.amazonaws.services.s3.model.S3ObjectSummary;
import com.amazonaws.util.EC2MetadataUtils;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.BackupEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.Configuration;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.RetentionEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.SnapshotEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.TaskEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.ConfigurationRepository;
import com.sungardas.enhancedsnapshots.components.impl.ConfigurationMediatorImpl;
import com.sungardas.enhancedsnapshots.dto.SystemConfiguration;
import com.sungardas.enhancedsnapshots.exception.EnhancedSnapshotsException;
import com.sungardas.enhancedsnapshots.service.SDFSStateService;
import com.sungardas.enhancedsnapshots.service.SystemService;
import com.sungardas.enhancedsnapshots.service.upgrade.SystemUpgrade;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.codehaus.jackson.map.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.DependsOn;

/**
 * Implementation for {@link SystemService}
 */
@DependsOn("CreateAppConfiguration")
public class SystemServiceImpl implements SystemService {
    private static final Logger LOG = LogManager.getLogger(SystemServiceImpl.class);

    private static final String CURRENT_VERSION = "0.0.2";
    private static final String LATEST_VERSION = "latest-version";
    private static final String INFO_URL = "http://com.sungardas.releases.s3.amazonaws.com/info";

    private static final String TEMP_DIRECTORY_PREFIX = "systemBackupFiles";
    private static final String INFO_FILE_NAME = "info";
    private static final String VERSION_KEY = "version";
    private static final String TEMP_FILE_SUFFIX = "ZIP";

    private static final String[] VOLUME_TYPE_OPTIONS = new String[]{VolumeType.Gp2.toString(), VolumeType.Io1.toString(), VolumeType.Standard.toString()};


    @Autowired
    private DynamoDBMapperConfig config;

    @Autowired
    private AmazonDynamoDB amazonDynamoDB;

    @Autowired
    private SystemUpgrade systemUpgrade;

    @Autowired
    private AmazonS3 amazonS3;

    @Autowired
    private ConfigurationRepository configurationRepository;

    @Autowired
    private ConfigurationMediatorImpl configurationMediator;

    @Autowired
    private SDFSStateService sdfsStateService;

    @Value("${enhancedsnapshots.bucket.name.prefix}")
    private String bucketNamePrefix;


    private final ObjectMapper objectMapper = new ObjectMapper();

    private DynamoDBMapper dynamoDBMapper;

    private Configuration currentConfiguration;

    @PostConstruct
    private void init() {
        dynamoDBMapper = new DynamoDBMapper(amazonDynamoDB, config);
        currentConfiguration = dynamoDBMapper.load(Configuration.class, getInstanceId());
        configurationMediator.setCurrentConfiguration(currentConfiguration);
    }


    @Override
    public void backup() {
        try {
            LOG.info("System backup started");
            Path tempDirectory = Files.createTempDirectory(TEMP_DIRECTORY_PREFIX);
            LOG.info("Add info file");
            addInfo(tempDirectory);
            LOG.info("Backup SDFS state");
            backupSDFS(tempDirectory);
            LOG.info("Backup files");
            storeFiles(tempDirectory);
            LOG.info("Backup db");
            backupDB(tempDirectory);
            LOG.info("Upload to S3");
            uploadToS3(tempDirectory);
            tempDirectory.toFile().delete();
            LOG.info("System backup finished");
        } catch (IOException e) {
            LOG.error("System backup failed");
            LOG.error(e);
            throw new EnhancedSnapshotsException(e);
        }
    }

    @Override
    public void restore() {
        try {
            Path tempDirectory = Files.createTempDirectory(TEMP_DIRECTORY_PREFIX);
            downloadFromS3(tempDirectory);
            systemUpgrade.upgrade(tempDirectory, getBackupVersion(tempDirectory));
            restoreSDFS(tempDirectory);
            restoreFiles(tempDirectory);
            restoreDB(tempDirectory);
        } catch (IOException e) {
            LOG.error("System restore failed");
            LOG.error(e);
            throw new EnhancedSnapshotsException(e);
        }
    }

    private String getBackupVersion(final Path tempDirectory) {
        Path infoFile = Paths.get(tempDirectory.toString(), INFO_FILE_NAME);
        if (infoFile.toFile().exists()) {
            try (FileInputStream fileInputStream = new FileInputStream(infoFile.toFile())) {
                HashMap<String, String> info = objectMapper.readValue(fileInputStream, HashMap.class);
                if (info.containsKey(VERSION_KEY)) {
                    return info.get(VERSION_KEY);
                } else {
                    LOG.error("Invalid info file formant");
                    throw new EnhancedSnapshotsException("Invalid info file formant");
                }
            } catch (IOException e) {
                LOG.error("Failed to parse info file");
                LOG.error(e);
                throw new EnhancedSnapshotsException(e);
            }
        }
        return "0.0.1";
    }

    private void addInfo(final Path tempDirectory) throws IOException {
        File dest = Paths.get(tempDirectory.toString(), INFO_FILE_NAME).toFile();
        Map<String, String> info = new HashMap<>();
        info.put(VERSION_KEY, CURRENT_VERSION);
        objectMapper.writeValue(dest, info);
    }

    private void backupDB(Path tempDirectory) throws IOException {
        storeTable(BackupEntry.class, tempDirectory);
        storeTable(Configuration.class, tempDirectory);
        storeTable(RetentionEntry.class, tempDirectory);
        storeTable(SnapshotEntry.class, tempDirectory);
        storeTable(TaskEntry.class, tempDirectory);
        storeTable(User.class, tempDirectory);
    }

    private void restoreDB(Path tempDirectory) throws IOException {
        restoreTable(BackupEntry.class, tempDirectory);
        restoreTable(Configuration.class, tempDirectory);
        restoreTable(RetentionEntry.class, tempDirectory);
        restoreTable(SnapshotEntry.class, tempDirectory);
        restoreTable(TaskEntry.class, tempDirectory);
        restoreTable(User.class, tempDirectory);
    }


    private void backupSDFS(final Path tempDirectory) throws IOException {
        copyToDirectory(Paths.get(currentConfiguration.getSdfsConfigPath()), tempDirectory);
        //TODO sdfscli --cloud-sync-fs
        sdfsStateService.stopSDFS();
        sdfsStateService.startSDFS();
    }

    private void restoreSDFS(final Path tempDirectory) throws IOException {
        sdfsStateService.stopSDFS();

        restoreFile(tempDirectory, Paths.get(currentConfiguration.getSdfsConfigPath()));
        //TODO sdfscli --cloud-sync-fs

        sdfsStateService.startSDFS();
    }

    private void storeFiles(Path tempDirectory) {
        //nginx certificates
        try {
            copyToDirectory(Paths.get(currentConfiguration.getNginxCertPath()), tempDirectory);
            copyToDirectory(Paths.get(currentConfiguration.getNginxKeyPath()), tempDirectory);
        } catch (IOException e) {
            LOG.warn("Nginx certificate not found");
        }
    }

    private void restoreFiles(Path tempDirectory) {
        //nginx certificates
        try {
            restoreFile(tempDirectory, Paths.get(currentConfiguration.getNginxCertPath()));
            restoreFile(tempDirectory, Paths.get(currentConfiguration.getNginxKeyPath()));
        } catch (IOException e) {
            LOG.warn("Nginx certificate not found");
        }
    }

    private void uploadToS3(final Path tempDirectory) throws IOException {
        // Compress to zip
        LOG.info("  -Compress files");
        File[] files = tempDirectory.toFile().listFiles();
        Path zipFile = Files.createTempFile(TEMP_DIRECTORY_PREFIX, TEMP_FILE_SUFFIX);
        try (FileOutputStream fos = new FileOutputStream(zipFile.toFile());
             ZipOutputStream zos = new ZipOutputStream(fos)) {
            for (File file : files) {
                zos.putNextEntry(new ZipEntry(file.getName()));
                Files.copy(file.toPath(), zos);
                zos.closeEntry();
            }
        }

        //Upload
        LOG.info("  -Upload");
        PutObjectRequest putObjectRequest = new PutObjectRequest(configurationMediator.getS3Bucket(),
                configurationMediator.getSdfsBackupFileName(), zipFile.toFile());
        amazonS3.putObject(putObjectRequest);

        zipFile.toFile().delete();
    }

    private void downloadFromS3(Path tempDirectory) throws IOException {
        // download
        GetObjectRequest getObjectRequest = new GetObjectRequest(configurationMediator.getS3Bucket(),
                configurationMediator.getSdfsBackupFileName());
        S3Object s3object = amazonS3.getObject(getObjectRequest);

        Path tempFile = Files.createTempFile(TEMP_DIRECTORY_PREFIX, TEMP_FILE_SUFFIX);
        Files.copy(s3object.getObjectContent(), tempFile, StandardCopyOption.REPLACE_EXISTING);

        //unzip
        try (FileInputStream fileInputStream = new FileInputStream(tempFile.toFile());
             ZipInputStream zipInputStream = new ZipInputStream(fileInputStream)) {
            ZipEntry entry;
            while ((entry = zipInputStream.getNextEntry()) != null) {
                Path dest = Paths.get(tempDirectory.toString(), entry.getName());
                Files.copy(zipInputStream, dest, StandardCopyOption.REPLACE_EXISTING);
            }
        }

        //cleanup
        tempFile.toFile().delete();
    }

    private void storeTable(Class tableClass, Path tempDirectory) throws IOException {
        LOG.info("Backup DB table: {}", tableClass.getSimpleName());
        File dest = Paths.get(tempDirectory.toString(), tableClass.getName()).toFile();
        List result = dynamoDBMapper.scan(tableClass, new DynamoDBScanExpression());
        objectMapper.writeValue(dest, result);
    }

    private void restoreTable(Class tableClass, Path tempDirectory) throws IOException {
        File src = Paths.get(tempDirectory.toString(), tableClass.getName()).toFile();
        try (FileInputStream fileInputStream = new FileInputStream(src)) {
            ArrayList data = objectMapper.readValue(fileInputStream, ArrayList.class);
            dynamoDBMapper.batchSave(data);
        } catch (IOException e) {
            LOG.warn("Table restore failed: {}", e.getLocalizedMessage());
        }
    }

    @Override
    public SystemConfiguration getSystemConfiguration() {
        SystemConfiguration configuration = new SystemConfiguration();

        configuration.setS3(new SystemConfiguration.S3());
        configuration.getS3().setBucketName(configurationMediator.getS3Bucket());

        configuration.setSdfs(new SystemConfiguration.SDFS());
        configuration.getSdfs().setMountPoint(configurationMediator.getSdfsMountPoint());
        configuration.getSdfs().setVolumeName(configurationMediator.getSdfsVolumeName());
        configuration.getSdfs().setVolumeSize(currentConfiguration.getSdfsSize());
        // user can only expand volume size
        configuration.getSdfs().setMinVolumeSize(currentConfiguration.getSdfsSize());
        configuration.getSdfs().setMaxVolumeSize(SDFSStateService.getMaxVolumeSize(true));

        configuration.getSdfs().setSdfsLocalCacheSize(currentConfiguration.getSdfsLocalCacheSize());
        configuration.getSdfs().setMaxSdfsLocalCacheSize(SDFSStateService.getFreeStorageSpace() + configurationMediator.getSdfsLocalCacheSizeWithoutMeasureUnit());
        configuration.getSdfs().setMinSdfsLocalCacheSize(configurationMediator.getSdfsLocalCacheSizeWithoutMeasureUnit());


        configuration.setEc2Instance(new SystemConfiguration.EC2Instance());
        configuration.getEc2Instance().setInstanceID(getInstanceId());

        configuration.setLastBackup(getBackupTime());
        configuration.setCurrentVersion(CURRENT_VERSION);
        configuration.setLatestVersion(getLatestVersion());

        SystemConfiguration.SystemProperties systemProperties = new SystemConfiguration.SystemProperties();
        systemProperties.setRestoreVolumeIopsPerGb(configurationMediator.getRestoreVolumeIopsPerGb());
        systemProperties.setRestoreVolumeType(configurationMediator.getRestoreVolumeType().toString());
        systemProperties.setTempVolumeIopsPerGb(configurationMediator.getTempVolumeIopsPerGb());
        systemProperties.setTempVolumeType(configurationMediator.getTempVolumeType().toString());
        systemProperties.setVolumeTypeOptions(VOLUME_TYPE_OPTIONS);
        systemProperties.setAmazonRetryCount(configurationMediator.getAmazonRetryCount());
        systemProperties.setAmazonRetrySleep(configurationMediator.getAmazonRetrySleep());
        systemProperties.setMaxQueueSize(configurationMediator.getMaxQueueSize());
        configuration.setSystemProperties(systemProperties);
        return configuration;
    }

    @Override
    public void setSystemConfiguration(SystemConfiguration configuration) {
        LOG.info("Updating system properties.");
        // update system properties
        currentConfiguration.setRestoreVolumeIopsPerGb(configuration.getSystemProperties().getRestoreVolumeIopsPerGb());
        currentConfiguration.setRestoreVolumeType(configuration.getSystemProperties().getRestoreVolumeType());
        currentConfiguration.setTempVolumeIopsPerGb(configuration.getSystemProperties().getTempVolumeIopsPerGb());
        currentConfiguration.setTempVolumeType(configuration.getSystemProperties().getTempVolumeType());
        currentConfiguration.setAmazonRetryCount(configuration.getSystemProperties().getAmazonRetryCount());
        currentConfiguration.setAmazonRetrySleep(configuration.getSystemProperties().getAmazonRetrySleep());
        currentConfiguration.setMaxQueueSize(configuration.getSystemProperties().getMaxQueueSize());

        // update sdfs setting
        currentConfiguration.setSdfsSize(configuration.getSdfs().getVolumeSize());
        currentConfiguration.setSdfsLocalCacheSize(configuration.getSdfs().getSdfsLocalCacheSize());

        // update bucket
        currentConfiguration.setS3Bucket(configuration.getS3().getBucketName());

        configurationRepository.save(currentConfiguration);

        configurationMediator.setCurrentConfiguration(currentConfiguration);
    }

    private String getLatestVersion() {
        try {
            URL infoURL = new URL(INFO_URL);
            Properties properties = new Properties();
            properties.load(infoURL.openStream());
            String latestVersion = properties.getProperty(LATEST_VERSION);
            if (latestVersion != null) {
                return latestVersion;
            }
        } catch (Exception e) {
        }
        return CURRENT_VERSION;
    }


    protected String getInstanceId() {
        return EC2MetadataUtils.getInstanceId();
    }

    //TODO: this should be stored in DB
    private Long getBackupTime() {
        ListObjectsRequest request = new ListObjectsRequest()
                .withBucketName(configurationMediator.getS3Bucket()).withPrefix(configurationMediator.getSdfsBackupFileName());
        List<S3ObjectSummary> list = amazonS3.listObjects(request).getObjectSummaries();
        if (list.size() > 0) {
            return list.get(0).getLastModified().getTime();
        } else {
            return null;
        }
    }

    private String[] getSuffixesInUse() {
        ArrayList<String> result = new ArrayList<>();
        List<Bucket> allBuckets = amazonS3.listBuckets();
        for (Bucket bucket : allBuckets) {
            if (bucket.getName().startsWith(bucketNamePrefix)) {
                result.add(bucket.getName().substring(bucketNamePrefix.length()));
            }
        }
        return result.toArray(new String[result.size()]);
    }

    private void restoreFile(Path tempDirectory, Path destPath) throws IOException {
        Path fileName = destPath.getFileName();
        Files.copy(Paths.get(tempDirectory.toString(), fileName.toString()), destPath, StandardCopyOption.REPLACE_EXISTING);
    }

    private void copyToDirectory(Path src, Path dest) throws IOException {
        Path srcFileName = src.getFileName();
        Files.copy(src, Paths.get(dest.toString(), srcFileName.toString()), StandardCopyOption.REPLACE_EXISTING);
    }
}

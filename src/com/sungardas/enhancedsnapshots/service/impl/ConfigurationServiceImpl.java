package com.sungardas.enhancedsnapshots.service.impl;

import java.net.URL;
import java.util.List;
import java.util.Properties;

import javax.annotation.PostConstruct;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.ec2.model.VolumeType;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.ListObjectsRequest;
import com.amazonaws.services.s3.model.S3ObjectSummary;
import com.amazonaws.util.EC2MetadataUtils;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.Configuration;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.ConfigurationRepository;
import com.sungardas.enhancedsnapshots.dto.SystemConfiguration;
import com.sungardas.enhancedsnapshots.service.ConfigurationService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile("prod")
public class ConfigurationServiceImpl implements ConfigurationService {

    private static final String CURRENT_VERSION = "0.0.1";
    private static final String LATEST_VERSION = "latest-version";
    private static final String INFO_URL = "http://com.sungardas.releases.s3.amazonaws.com/info";

    @Autowired
    private ConfigurationRepository configurationRepository;
    @Autowired
    @Qualifier("dynamoDB")
    private AmazonDynamoDB dynamoDB;
    @Autowired
    private AmazonS3 amazonS3;

    private String[] volumeTypeOptions = new String[]{VolumeType.Gp2.toString(), VolumeType.Io1.toString(), VolumeType.Standard.toString()};
    private Configuration currentConfiguration;


    @PostConstruct
    private void init() {
        // we need to use dynamoDbClient without retry interceptor proxy at this step since there is a cycle dependency
        // retryInterceptor -> configurationService -> configurationRepository -> retryInterceptor
        currentConfiguration = new DynamoDBMapper(dynamoDB)
                .load(Configuration.class, getInstanceId());
    }

    @Override
    public SystemConfiguration getSystemConfiguration() {
        SystemConfiguration configuration = new SystemConfiguration();

        configuration.setS3(new SystemConfiguration.S3());
        configuration.getS3().setBucketName(getS3Bucket());

        configuration.setSdfs(new SystemConfiguration.SDFS());
        configuration.getSdfs().setMountPoint(getSdfsMountPoint());
        configuration.getSdfs().setVolumeName(getSdfsVolumeName());
        configuration.getSdfs().setVolumeSize(getSdfsVolumeSize());

        configuration.setEc2Instance(new SystemConfiguration.EC2Instance());
        configuration.getEc2Instance().setInstanceID(EC2MetadataUtils.getInstanceId());

        configuration.setLastBackup(getBackupTime());
        configuration.setCurrentVersion(CURRENT_VERSION);
        configuration.setLatestVersion(getLatestVersion());

        SystemConfiguration.SystemProperties systemProperties = new SystemConfiguration.SystemProperties();
        systemProperties.setRestoreVolumeIopsPerGb(getRestoreVolumeIopsPerGb());
        systemProperties.setRestoreVolumeType(getRestoreVolumeType().toString());
        systemProperties.setTempVolumeIopsPerGb(getTempVolumeIopsPerGb());
        systemProperties.setTempVolumeType(getTempVolumeType().toString());
        systemProperties.setVolumeTypeOptions(volumeTypeOptions);
        configuration.setSystemProperties(systemProperties);
        return configuration;
    }

    @Override
    public void setSystemProperties(SystemConfiguration.SystemProperties systemProperties) {
        currentConfiguration.setRestoreVolumeIopsPerGb(systemProperties.getRestoreVolumeIopsPerGb());
        currentConfiguration.setRestoreVolumeType(systemProperties.getRestoreVolumeType());
        currentConfiguration.setTempVolumeIopsPerGb(systemProperties.getTempVolumeIopsPerGb());
        currentConfiguration.setTempVolumeType(systemProperties.getTempVolumeType());
        configurationRepository.save(currentConfiguration);
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

    @Override
    public String getRegion() {
        return currentConfiguration.getEc2Region();
    }

    @Override
    public String getS3Bucket() {
        return currentConfiguration.getS3Bucket();
    }

    @Override
    public String getConfigurationId() {
        return currentConfiguration.getConfigurationId();
    }

    @Override
    public int getAmazonRetryCount() {
        return currentConfiguration.getAmazonRetryCount();
    }

    @Override
    public int getAmazonRetrySleep() {
        return currentConfiguration.getAmazonRetrySleep();
    }

    @Override
    public int getMaxQueueSize() {
        return currentConfiguration.getMaxQueueSize();
    }

    @Override
    public String getRetentionCronExpression() {
        return currentConfiguration.getRetentionCronExpression();
    }

    @Override
    public int getWorkerDispatcherPollingRate() {
        return currentConfiguration.getWorkerDispatcherPollingRate();
    }

    @Override
    public String getTempVolumeType() {
        return currentConfiguration.getTempVolumeType();
    }

    @Override
    public int getTempVolumeIopsPerGb() {
        return currentConfiguration.getTempVolumeIopsPerGb();
    }

    @Override
    public String getRestoreVolumeType() {
        return currentConfiguration.getRestoreVolumeType();
    }

    @Override
    public int getRestoreVolumeIopsPerGb() {
        return currentConfiguration.getRestoreVolumeIopsPerGb();
    }

    @Override
    public String getSdfsVolumeName() {
        return currentConfiguration.getSdfsVolumeName();
    }

    @Override
    public String getSdfsMountPoint() {
        return currentConfiguration.getSdfsMountPoint();
    }

    @Override
    public String getSdfsLocalCacheSize() {
        return currentConfiguration.getSdfsLocalCacheSize();
    }

    @Override
    public String getSdfsVolumeSize() {
        return currentConfiguration.getSdfsSize()+"GB";
    }

    @Override
    public String getSdfsConfigPath() {
        return currentConfiguration.getSdfsConfigPath();
    }

    @Override
    public String getSdfsBackupFileName() {
        return currentConfiguration.getSdfsBackupFileName();
    }

    @Override
    public int getWaitTimeBeforeNewSyncWithAWS() {
        return currentConfiguration.getWaitTimeBeforeNewSyncWithAWS();
    }

    @Override
    public int getMaxWaitTimeToDetachVolume() {
        return currentConfiguration.getMaxWaitTimeToDetachVolume();
    }

    protected String getInstanceId() {
        return EC2MetadataUtils.getInstanceId();
    }

    //TODO: this should be stored in DB
    private Long getBackupTime() {
        ListObjectsRequest request = new ListObjectsRequest()
                .withBucketName(getS3Bucket()).withPrefix(getSdfsBackupFileName());
        List<S3ObjectSummary> list = amazonS3.listObjects(request).getObjectSummaries();
        if (list.size() > 0) {
            return list.get(0).getLastModified().getTime();
        } else {
            return null;
        }
    }
}

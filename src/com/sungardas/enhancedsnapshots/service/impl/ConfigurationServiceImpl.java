package com.sungardas.enhancedsnapshots.service.impl;

import java.net.URL;
import java.util.Properties;

import com.amazonaws.services.ec2.model.VolumeType;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.Configuration;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.ConfigurationRepository;
import com.sungardas.enhancedsnapshots.dto.SystemConfiguration;
import com.sungardas.enhancedsnapshots.service.ConfigurationService;
import com.sungardas.enhancedsnapshots.service.SDFSStateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class ConfigurationServiceImpl implements ConfigurationService {

    private static final String CURRENT_VERSION = "0.0.1";
    private static final String LATEST_VERSION = "latest-version";
    private static final String INFO_URL = "http://com.sungardas.releases.s3.amazonaws.com/info";
    @Autowired
    private ConfigurationRepository configurationRepository;
    private Configuration currentConfiguration;
    @Autowired
    private SDFSStateService sdfsStateService;
    @Value("${sungardas.worker.configuration}")
    private String instanceId;
    @Value("${amazon.s3.bucket}")
    private String s3BucketName;
    @Value("${amazon.sdfs.size}")
    private String volumeSize;
    private String[] volumeTypeOptions = new String[]{VolumeType.Gp2.toString(), VolumeType.Io1.toString(), VolumeType.Standard.toString()};


    @Override
    public Configuration getConfiguration() {
        if (currentConfiguration == null) {
            currentConfiguration = configurationRepository.findOne(instanceId);
        }
        return currentConfiguration;
    }

    @Override
    public SystemConfiguration getSystemConfiguration() {
        SystemConfiguration configuration = new SystemConfiguration();

        configuration.setS3(new SystemConfiguration.S3());
        configuration.getS3().setBucketName(s3BucketName);

        configuration.setSdfs(new SystemConfiguration.SDFS());
        configuration.getSdfs().setMountPoint(currentConfiguration.getSdfsMountPoint());
        configuration.getSdfs().setVolumeName(currentConfiguration.getSdfsVolumeName());
        //TODO change to read from DB SNAP-357
        configuration.getSdfs().setVolumeSize(volumeSize);

        configuration.setEc2Instance(new SystemConfiguration.EC2Instance());
        configuration.getEc2Instance().setInstanceID(instanceId);

        configuration.setLastBackup(sdfsStateService.getBackupTime());
        configuration.setCurrentVersion(CURRENT_VERSION);
        configuration.setLatestVersion(getLatestVersion());

        SystemConfiguration.SystemProperties systemProperties = new SystemConfiguration.SystemProperties();
        systemProperties.setRestoreVolumeIopsPerGb(currentConfiguration.getRestoreVolumeIopsPerGb());
        systemProperties.setRestoreVolumeType(currentConfiguration.getRestoreVolumeType().toString());
        systemProperties.setTempVolumeIopsPerGb(currentConfiguration.getTempVolumeIopsPerGb());
        systemProperties.setTempVolumeType(currentConfiguration.getTempVolumeType().toString());
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
    public void reload() {
        currentConfiguration = null;
    }
}

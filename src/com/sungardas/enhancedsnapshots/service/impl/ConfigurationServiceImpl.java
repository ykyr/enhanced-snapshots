package com.sungardas.enhancedsnapshots.service.impl;

import com.amazonaws.services.ec2.model.VolumeType;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.Configuration;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.ConfigurationRepository;
import com.sungardas.enhancedsnapshots.dto.SystemConfiguration;
import com.sungardas.enhancedsnapshots.service.ConfigurationService;
import com.sungardas.enhancedsnapshots.service.SDFSStateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.net.URL;
import java.util.Properties;

@Service
public class ConfigurationServiceImpl implements ConfigurationService {

    private static final String CURRENT_VERSION = "0.0.1";
    private static final String LATEST_VERSION = "latest-version";
    private static final String INFO_URL = "http://com.sungardas.releases.s3.amazonaws.com/info";
    @Autowired
    private ConfigurationRepository configurationRepository;
    private Configuration currectConfiguration;
    @Autowired
    private SDFSStateService sdfsStateService;
    @Value("${sungardas.worker.configuration}")
    private String instanceId;
    @Value("${amazon.s3.bucket}")
    private String s3BucketName;
    @Value("${enhancedsnapshots.sdfs.default.size}")
    private String defaultVolumeSize;
    private String[] volumeTypeOptions = new String[]{VolumeType.Gp2.toString(), VolumeType.Io1.toString(), VolumeType.Standard.toString()};


    @Override
    public Configuration getConfiguration() {
        if (currectConfiguration == null) {
            currectConfiguration = configurationRepository.findOne(instanceId);
        }
        return currectConfiguration;
    }

    @Override
    public SystemConfiguration getSystemConfiguration() {
        SystemConfiguration configuration = new SystemConfiguration();

        configuration.setS3(new SystemConfiguration.S3());
        configuration.getS3().setBucketName(s3BucketName);

        configuration.setSdfs(new SystemConfiguration.SDFS());
        configuration.getSdfs().setMountPoint(currectConfiguration.getSdfsMountPoint());
        configuration.getSdfs().setVolumeName(currectConfiguration.getSdfsVolumeName());
        configuration.getSdfs().setVolumeSize(defaultVolumeSize);

        configuration.setEc2Instance(new SystemConfiguration.EC2Instance());
        configuration.getEc2Instance().setInstanceID(instanceId);

        configuration.setLastBackup(sdfsStateService.getBackupTime());
        configuration.setCurrentVersion(CURRENT_VERSION);
        configuration.setLatestVersion(getLatestVersion());

        SystemConfiguration.SystemProperties systemProperties = new SystemConfiguration.SystemProperties();
        systemProperties.setRestoreVolumeIopsPerGb(currectConfiguration.getRestoreVolumeIopsPerGb());
        systemProperties.setRestoreVolumeType(currectConfiguration.getRestoreVolumeType().toString());
        systemProperties.setTempVolumeIopsPerGb(currectConfiguration.getTempVolumeIopsPerGb());
        systemProperties.setTempVolumeType(currectConfiguration.getTempVolumeType().toString());
        systemProperties.setVolumeTypeOptions(volumeTypeOptions);
        configuration.setSystemProperties(systemProperties);
        return configuration;
    }

    @Override
    public void setSystemProperties(SystemConfiguration.SystemProperties systemProperties) {
        currectConfiguration.setRestoreVolumeIopsPerGb(systemProperties.getRestoreVolumeIopsPerGb());
        currectConfiguration.setRestoreVolumeType(systemProperties.getRestoreVolumeType());
        currectConfiguration.setTempVolumeIopsPerGb(systemProperties.getTempVolumeIopsPerGb());
        currectConfiguration.setTempVolumeType(systemProperties.getTempVolumeType());
        configurationRepository.save(currectConfiguration);
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
        currectConfiguration = null;
    }
}

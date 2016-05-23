package com.sungardas.init;

import java.util.ArrayList;
import java.util.List;

import javax.validation.constraints.NotNull;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;
import com.sungardas.enhancedsnapshots.exception.ConfigurationException;
import com.sungardas.enhancedsnapshots.service.SDFSStateService;
import org.springframework.beans.factory.annotation.Value;

class InitConfigurationServiceDev implements InitConfigurationService {


    @Value("${enhancedsnapshots.bucket.name.prefix}")
    private String enhancedSnapshotBucketPrefix;

    @Override
    public void removeProperties() {

    }

    @Override
    public InitConfigurationDto getInitConfigurationDto() {
        InitConfigurationDto config = new InitConfigurationDto();
        List<InitConfigurationDto.S3> names = new ArrayList<>();
        names.add(new InitConfigurationDto.S3("com.sungardas.enhancedsnapshots.S0", false));
        names.add(new InitConfigurationDto.S3("com.sungardas.enhancedsnapshots.S1", true));
        names.add(new InitConfigurationDto.S3("com.sungardas.enhancedsnapshots.S2", true));

        InitConfigurationDto.SDFS sdfs = new InitConfigurationDto.SDFS();
        sdfs.setCreated(true);
        sdfs.setMountPoint("/mnt/awspool");
        sdfs.setVolumeName("awspool");
        sdfs.setVolumeSize("40");
        sdfs.setMinVolumeSize("10");
        sdfs.setMaxVolumeSize("2000");
        sdfs.setSdfsLocalCacheSize(1);
        sdfs.setMinSdfsLocalCacheSize(0);
        sdfs.setMaxSdfsLocalCacheSize(3);

        InitConfigurationDto.DB db = new InitConfigurationDto.DB();
        db.setValid(true);
        db.setAdminExist(true);


        config.setS3(names);
        config.setSdfs(sdfs);
        config.setDb(db);
        config.setImmutableBucketNamePrefix(enhancedSnapshotBucketPrefix);
        config.setSuffixesInUse(new String[]{"i-1111", "i-2222", "i-4444"});

        return config;
    }

    @Override
    public boolean propertyFileExists() {
        return false;
    }

    @Override
    public boolean checkDefaultUser(String login, String password) {
        return true;
    }

    @Override
    public String getInstanceId() {
        return "DEV";
    }

    @Override
    public void configureAWSLogAgent() {
    }

    @Override
    public void validateVolumeSize(final int volumeSize) {
        int min = 10;
        int max = 2000;
        if (volumeSize < min || volumeSize > max) {
            throw new ConfigurationException("Invalid volume size");
        }
    }

    @Override
    public void storePropertiesEditableFromConfigFile() {
    }

    @Override
    public void setUser(User user) {

    }

    @Override
    public void createDBAndStoreSettings(final InitController.ConfigDto config) {

    }

    @Override
    public void syncSettingsInDbAndConfigFile() {

    }
}

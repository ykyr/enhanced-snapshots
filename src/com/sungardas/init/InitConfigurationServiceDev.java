package com.sungardas.init;

import java.util.ArrayList;
import java.util.List;

import javax.validation.constraints.NotNull;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;
import com.sungardas.enhancedsnapshots.exception.ConfigurationException;

class InitConfigurationServiceDev implements InitConfigurationService {


    @Override
    public void removeProperties() {

    }

    @Override
    public InitConfigurationDto getInitConfigurationDto() {
        InitConfigurationDto config = new InitConfigurationDto();
        List<InitConfigurationDto.S3> names = new ArrayList<>();
        names.add(new InitConfigurationDto.S3("S0", false));
        names.add(new InitConfigurationDto.S3("S1", true));
        names.add(new InitConfigurationDto.S3("S2", true));

        InitConfigurationDto.SDFS sdfs = new InitConfigurationDto.SDFS();
        sdfs.setCreated(true);
        sdfs.setMountPoint("/mnt/awspool");
        sdfs.setVolumeName("awspool");
        sdfs.setVolumeSize("40");
        sdfs.setMinVolumeSize("10");
        sdfs.setMaxVolumeSize("2000");

        InitConfigurationDto.DB db = new InitConfigurationDto.DB();
        db.setValid(true);
        db.setAdminExist(true);


        config.setS3(names);
        config.setSdfs(sdfs);
        config.setDb(db);

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
    public void validateVolumeSize(final String volumeSize) {
        int size = Integer.parseInt(volumeSize);
        int min = 10;
        int max = 2000;
        if (size < min || size > max) {
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

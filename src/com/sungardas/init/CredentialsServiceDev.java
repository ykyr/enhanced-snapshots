package com.sungardas.init;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;

import javax.validation.constraints.NotNull;
import java.util.ArrayList;
import java.util.List;

class CredentialsServiceDev implements CredentialsService {

    @Override
    public void setCredentialsIfValid(@NotNull CredentialsDto credentials) {

    }

    @Override
    public void removeCredentials() {

    }

    @Override
    public boolean areCredentialsValid() {
        return true;
    }

    @Override
    public boolean credentialsAreProvided() {
        return false;
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
        return true;
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
    public void storePropertiesEditableFromConfigFile() {
    }

    @Override
    public void setUser(User user) {

    }

    @Override
    public void setInitConfigurationDto(InitConfigurationDto initConfigurationDto) {

    }

    @Override
    public void createDBAndStoreSettings() {

    }

    @Override
    public void syncSettingsInDbAndConfigFile() {

    }
}

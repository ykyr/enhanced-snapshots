package com.sungardas.init;

import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;
import org.apache.commons.io.FileUtils;

import javax.validation.constraints.NotNull;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;

class CredentialsServiceDev implements CredentialsService {

    @Override
    public void setCredentialsIfValid(@NotNull CredentialsDto credentials) {

    }

    @Override
    public void storeCredentials() {

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
        db.setAdminExist(false);


        config.setS3(names);
        config.setSdfs(sdfs);
        config.setDb(db);

        return config;
    }

    @Override
    public boolean isAwsPropertyFileExists() {
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


}

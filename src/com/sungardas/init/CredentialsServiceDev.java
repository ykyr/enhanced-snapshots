package com.sungardas.init;

import com.amazonaws.auth.AWSCredentials;
import com.sungardas.snapdirector.dto.InitConfigurationDto;

import javax.validation.constraints.NotNull;

public class CredentialsServiceDev implements CredentialsService {

    @Override
    public void setCredentialsIfValid(@NotNull CredentialsDto credentials) {

    }

    @Override
    public void storeCredentials() {

    }

    @Override
    public boolean areCredentialsValid() {
        return true;
    }

    @Override
    public InitConfigurationDto getInitConfigurationDto() {
        InitConfigurationDto config = new InitConfigurationDto();

        InitConfigurationDto.S3 s3 = new InitConfigurationDto.S3();
        s3.setBucketName("com.sungardas.snapdirector_i-12f5a345");
        s3.setCreated(true);

        InitConfigurationDto.SDFS sdfs = new InitConfigurationDto.SDFS();
        sdfs.setCreated(true);
        sdfs.setMountPoint("/mnt/awspool");
        sdfs.setVolumeName("awspool");
        sdfs.setVolumeSize("40");

        InitConfigurationDto.Queue queue = new InitConfigurationDto.Queue();
        queue.setQueueName("snapdirector_i-12f5a345");
        queue.setCreated(true);

        InitConfigurationDto.DB db = new InitConfigurationDto.DB();
        db.setValid(false);

        config.setS3(s3);
        config.setSdfs(sdfs);
        config.setQueue(queue);
        config.setDb(db);

        return config;
    }

    @Override
    public boolean isAwsPropertyFileExists() {
        return false;
    }

    @Override
    public AWSCredentials getCredentials() {
        return null;
    }

    @Override
    public boolean checkDefaultUser(String login, String password) {
        return true;
    }
}

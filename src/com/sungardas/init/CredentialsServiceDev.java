package com.sungardas.init;

import com.amazonaws.auth.AWSCredentials;
import com.sungardas.snapdirector.dto.InitConfigurationDto;

import javax.validation.constraints.NotNull;
import java.io.File;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

class CredentialsServiceDev implements CredentialsService {
    private final String catalinaHomeEnvPropName = "catalina.home";
    private final String confFolderName = "conf";
    private final String propFileName = "amazon.properties";

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
    public List<String> getBucketsWithSdfsMetadata() {
        return new ArrayList<>();
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
        return getPropertyFile().exists();
    }

    @Override
    public AWSCredentials getCredentials() {
        return null;
    }

    @Override
    public boolean checkDefaultUser(String login, String password) {
        return true;
    }

    private File getPropertyFile() {
        return Paths.get(System.getProperty(catalinaHomeEnvPropName), confFolderName, propFileName).toFile();
    }
}

package com.sungardas.init;

import com.amazonaws.auth.AWSCredentials;
import com.sungardas.snapdirector.dto.InitConfigurationDto;

import javax.validation.constraints.NotNull;
import java.util.List;

interface CredentialsService {
    void setCredentialsIfValid(@NotNull CredentialsDto credentials);

    void storeCredentials();

    boolean areCredentialsValid();

    List<String> getBucketsWithSdfsMetadata();

    InitConfigurationDto getInitConfigurationDto();

    boolean isAwsPropertyFileExists();

    AWSCredentials getCredentials();

    boolean checkDefaultUser(String login, String password);
}

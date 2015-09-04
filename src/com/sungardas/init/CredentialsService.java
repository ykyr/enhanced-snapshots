package com.sungardas.init;

import com.amazonaws.auth.AWSCredentialsProvider;
import com.sungardas.snapdirector.dto.InitConfigurationDto;

public interface CredentialsService extends AWSCredentialsProvider {
    void setCredentials(String acessKey, String secretKey);

    void storeCredentials();

    boolean areCredentialsValid();

    InitConfigurationDto getInitConfigurationDto();

    boolean areStoredCredentialsValid();

    boolean isAwsPropertyFileExists();
}

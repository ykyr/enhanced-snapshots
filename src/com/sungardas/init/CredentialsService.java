package com.sungardas.init;

import com.amazonaws.auth.AWSCredentialsProvider;

public interface CredentialsService extends AWSCredentialsProvider {
    void setCredentials(String acessKey, String secretKey);

    void storeCredentials();

    boolean areCredentialsValid();

    boolean areStoredCredentialsValid();

    boolean isAwsPropertyFileExists();
}

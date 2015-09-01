package com.sungardas.snapdirector.service;

import com.amazonaws.auth.AWSCredentialsProvider;

public interface CredentialsService extends AWSCredentialsProvider {
    void setCredentials(String acessKey, String secretKey);

    boolean areCredentialsValid();

    boolean isAwsPropertyFileExists();
}

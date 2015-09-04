package com.sungardas.init;

public class CredentialsDto {
    private String awsPublicKey;

    private String awsSecretKey;

    public String getAwsPublicKey() {
        return awsPublicKey;
    }

    public void setAwsPublicKey(String awsPublicKey) {
        this.awsPublicKey = awsPublicKey;
    }

    public String getAwsSecretKey() {
        return awsSecretKey;
    }

    public void setAwsSecretKey(String awsSecretKey) {
        this.awsSecretKey = awsSecretKey;
    }
}

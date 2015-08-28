package com.sungardas.snapdirector.service;

public interface InitializationService {
	boolean ValidAWSCredentialsAreProvided();

    boolean AWSCredentialsAreValid(String accessKey, String secretKey);

    boolean isSystemInitialized();

    boolean checkDefaultUser(String login, String passwd);

    boolean isDbStructureValid();

    boolean isConfigurationExists();

    boolean isQueueExists();

    boolean isSdfsConfigured();

    boolean isAdminUserExists();
}

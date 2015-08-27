package com.sungardas.snapdirector.service;

public interface InitializationService {
    boolean AWSCredentialsAreValid();

    boolean isSystemInitialized();

    boolean checkDefaultUser(String login, String passwd);

    boolean isDbStructureValid();

    boolean isConfigurationExists();

    boolean isQueueExists();

    boolean isSdfsConfigured();

    boolean isAdminUserExists();
}

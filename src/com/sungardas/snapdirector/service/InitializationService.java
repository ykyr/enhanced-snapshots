package com.sungardas.snapdirector.service;

public interface InitializationService {
    boolean AWSCredentialsAreValid();

    boolean isSystemInitialized();

    boolean checkDefaultUser(String login, String passwd);

    boolean checkDbStructureIsValid();

    boolean checkConfigurationExists();

    boolean checkQueueExists();

    boolean checkSdfs();

    boolean checkAdminUserExists();
}

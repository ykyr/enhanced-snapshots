package com.sungardas.snapdirector.service;

public interface InitializationService {

    boolean isSystemInitialized();

    boolean isDbStructureValid();

    boolean isConfigurationExists();

    boolean isQueueExists();

    boolean isSdfsConfigured();

    boolean isAdminUserExists();
}

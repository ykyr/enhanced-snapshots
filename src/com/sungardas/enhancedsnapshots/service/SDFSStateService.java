package com.sungardas.enhancedsnapshots.service;

public interface SDFSStateService {

    void backupState();

    void backupState(String taskId);

    void restoreState();

    boolean containsSdfsMetadata(String sBucket);

    Long getBackupTime();

    void shutdownSDFS(String size, String bucketName);

    void startupSDFS(String size, String bucketName, Boolean isRestore);
}

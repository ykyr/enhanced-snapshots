package com.sungardas.snapdirector.service;

public interface SDFSStateService {

    void backupState();

    void restoreState();

    boolean containsSdfsMetadata(String sBucket);

    void startupSDFS(String size, String bucketName);

    void shutdownSDFS(String size, String bucketName);

}

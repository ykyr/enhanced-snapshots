package com.sungardas.snapdirector.service;

public interface SDFSStateService {

    void backupState();

    void restoreState();

    boolean containsSdfsMetadata(String sBucket);

    void createSDFS(String size, String bucketName);
}

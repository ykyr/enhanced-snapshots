package com.sungardas.enhancedsnapshots.service;

public interface SDFSStateService {

    void backupState(String taskId);

    boolean containsSdfsMetadata(String sBucket);

    Long getBackupTime();

    void reconfigureAndRestartSDFS();

    void restoreSDFS();

    void startSDFS();

    void stopSDFS();

    boolean sdfsIsRunning();
}

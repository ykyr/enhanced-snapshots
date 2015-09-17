package com.sungardas.snapdirector.service;

public interface SnapshotService {

    String getSnapshotId(String volumeId, String instancreId);

    void saveSnapshot(String volumeId, String instanceId, String snapshotId);
}

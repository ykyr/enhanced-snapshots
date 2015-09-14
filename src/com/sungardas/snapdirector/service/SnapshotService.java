package com.sungardas.snapdirector.service;

public interface SnapshotService {

    void addSnapshot(String snapshotId, String volumeId);

    void removeSnapshot(String snapshotId);
}

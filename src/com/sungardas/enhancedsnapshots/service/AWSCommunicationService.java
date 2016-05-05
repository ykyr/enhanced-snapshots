package com.sungardas.enhancedsnapshots.service;

import com.amazonaws.services.ec2.model.*;

import java.util.List;

public interface AWSCommunicationService {

    Snapshot createSnapshot(Volume volume);

    void deleteSnapshot(String snapshotId);

    void cleanupSnapshots(String volumeId, String snapshotIdToLeave);

    Snapshot waitForCompleteState(Snapshot snapshot);

    Snapshot syncSnapshot(Snapshot snapshot);

    Volume waitForAvailableState(Volume volume);

    Volume syncVolume(Volume volume);

    /**
     * iopsPerGb paramenter is only required for io1 volume type, for other volume types it will be skipped
     */
    Volume createVolumeFromSnapshot(String snapshotId, String availabilityZoneName, VolumeType type, int iopsPerGb);

    void deleteVolume(Volume volume);

    void attachVolume(Instance instance, Volume volume);

	Volume getVolume(String volumeId);

    List<AvailabilityZone> describeAvailabilityZonesForCurrentRegion();

    String getCurrentAvailabilityZone();

    void createTemporaryTag(String resourceId, String description);

    void deleteTemporaryTag(String resourceId);

    Volume createVolume(int size, VolumeType type);

    Volume createIO1Volume(int size, int iopsPerGb);

    Instance getInstance(String instanceId);

    void detachVolume(Volume volume);

    void setResourceName(String resourceid, String value);

    void addTag(String resourceId, String name, String value);

    Snapshot getSnapshot(String snapshotId);
}
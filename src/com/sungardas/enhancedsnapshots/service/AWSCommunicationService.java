package com.sungardas.enhancedsnapshots.service;

import com.amazonaws.services.ec2.model.AvailabilityZone;
import com.amazonaws.services.ec2.model.Instance;
import com.amazonaws.services.ec2.model.Snapshot;
import com.amazonaws.services.ec2.model.Volume;

import java.util.List;

public interface AWSCommunicationService {

    Snapshot createSnapshot(Volume volume);

    void deleteSnapshot(String snapshotId);

    void cleanupSnapshots(String volumeId, String snapshotIdToLeave);

    Snapshot waitForCompleteState(Snapshot snapshot);

    Snapshot syncSnapshot(Snapshot snapshot);

    Volume waitForAvailableState(Volume volume);

    Volume syncVolume(Volume volume);

    Volume createVolumeFromSnapshot(String snapshotId, String availabilityZoneName);

    Volume createVolumeFromSnapshot(Snapshot sourceSnapshot, String availabilityZoneName);

    void deleteVolume(Volume volume);

    void attachVolume(Instance instance, Volume volume);

	Volume getVolume(String volumeId);

    List<AvailabilityZone> describeAvailabilityZonesForCurrentRegion();

    void createTemporaryTag(String resourceId, String description);

    void deleteTemporaryTag(String resourceId);

    Volume createVolume(int size, int iiops, String type);

    Volume createIO1Volume(int size, int iops);

    Volume createGP2Volume(int size);

    Volume createStandardVolume(int size);

    Instance getInstance(String instanceId);

    void detachVolume(Volume volume);

    void setResourceName(String resourceid, String value);
}
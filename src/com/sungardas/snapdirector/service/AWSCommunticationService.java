package com.sungardas.snapdirector.service;

import java.util.List;

import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.model.Instance;
import com.amazonaws.services.ec2.model.Snapshot;
import com.amazonaws.services.ec2.model.Volume;
import com.amazonaws.services.ec2.model.VolumeAttachment;

public interface AWSCommunticationService {

	Snapshot createSnapshot(Volume volume);

	void deleteSnapshot(Volume associatedVolume);

	void deleteSnapshot(String snapshotId);
	
	void cleanupSnapshots(String volumeId, String snapshotIdToLeave);
	
	void cleanupSnapshots(Volume volume, Snapshot snapshotToLeave);

	Snapshot waitForCompleteState(Snapshot snapshot);

	Snapshot syncSnapshot(Snapshot snapshot);

	Volume waitForAvailableState(Volume volume);

	Volume syncVolume(Volume volume);

	Volume createVolumeFromSnapshot(String snapshotId, String availabilityZoneName);

	Volume createVolumeFromSnapshot(Snapshot sourceSnapshot, String availabilityZoneName);

	void deleteVolume(Volume volume);

	void attachVolume(Instance instance, Volume volume);

	List<Instance> getInstanceList();

	List<Volume> getVolumeList();

	Volume getVolume(String volumeId);

	Volume createVolume(int size, int iiops, String type);

	Volume createIO1Volume(int size, int iops);

	Volume createGP2Volume(int size);

	Volume createStandardVolume(int size);

	Instance getInstance(String instanceId);

	void detachVolume(Volume volume);

	void setResourceName(String resourceid, String value);
}

package com.sungardas.snapdirector.service.impl;

import static java.lang.String.format;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.model.AttachVolumeRequest;
import com.amazonaws.services.ec2.model.AttachVolumeResult;
import com.amazonaws.services.ec2.model.CreateSnapshotRequest;
import com.amazonaws.services.ec2.model.CreateSnapshotResult;
import com.amazonaws.services.ec2.model.CreateTagsRequest;
import com.amazonaws.services.ec2.model.CreateVolumeRequest;
import com.amazonaws.services.ec2.model.CreateVolumeResult;
import com.amazonaws.services.ec2.model.DeleteSnapshotRequest;
import com.amazonaws.services.ec2.model.DeleteVolumeRequest;
import com.amazonaws.services.ec2.model.DescribeInstancesRequest;
import com.amazonaws.services.ec2.model.DescribeInstancesResult;
import com.amazonaws.services.ec2.model.DescribeSnapshotsRequest;
import com.amazonaws.services.ec2.model.DescribeSnapshotsResult;
import com.amazonaws.services.ec2.model.DescribeVolumesRequest;
import com.amazonaws.services.ec2.model.DescribeVolumesResult;
import com.amazonaws.services.ec2.model.DetachVolumeRequest;
import com.amazonaws.services.ec2.model.DetachVolumeResult;
import com.amazonaws.services.ec2.model.Instance;
import com.amazonaws.services.ec2.model.InstanceBlockDeviceMapping;
import com.amazonaws.services.ec2.model.Reservation;
import com.amazonaws.services.ec2.model.Snapshot;
import com.amazonaws.services.ec2.model.SnapshotState;
import com.amazonaws.services.ec2.model.Tag;
import com.amazonaws.services.ec2.model.Volume;
import com.amazonaws.services.ec2.model.VolumeState;
import com.sungardas.snapdirector.dto.SnapshotDto;
import com.sungardas.snapdirector.service.AWSCommunticationService;
import com.sungardas.snapdirector.service.SnapshotService;

@Service
public class AWSCommunticationServiceImpl implements AWSCommunticationService {

	private static final Logger LOG = LogManager.getLogger(AWSCommunticationServiceImpl.class);

	@Autowired
	private SnapshotService snapshotService;
	
	@Autowired
	private AmazonEC2 ec2client;
	
	@Value("${sungardas.worker.configuration}")
	private String configurationId;
	

	@Value("${sungardas.restore.snapshot.attempts:5}")
	private int retryRestoreAttempts;

	@Value("${sungardas.restore.snapshot.timeout:5}")
	private int retryRestoreTimeout;

	
	@Override
	public Volume createVolume(int size, int iiops, String type) {
		String availabilityZone = getInstance(configurationId).getPlacement().getAvailabilityZone();
		
		CreateVolumeRequest createVolumeRequest = new CreateVolumeRequest().
				withSize(size).withVolumeType(type).withAvailabilityZone(availabilityZone);
		if (iiops>0) {
			createVolumeRequest = createVolumeRequest.withIops(iiops);
		}
		Volume result = ec2client.createVolume(createVolumeRequest).getVolume();
		return result;
	}
	

	@Override
	public Volume createStandardVolume(int size) {
		return createVolume(size, 0, "standard");
	}

	@Override
	public Volume createGP2Volume(int size) {
		return createVolume(size, 0, "gp2");
	}

	@Override
	public Volume createIO1Volume(int size, int iops) {
		return createVolume(size, iops, "io1");
	}

	@Override
	public Snapshot createSnapshot(Volume volume) {
		SimpleDateFormat formatter = new SimpleDateFormat("yyyy.MM.dd'_T'hh:mm:ss");

		String volumeId = volume.getVolumeId();
		LOG.info(format("Starting creating snapshot for %s", volumeId));
		CreateSnapshotRequest snapshotRequest = new CreateSnapshotRequest(volumeId, volumeId + "__"
				+ formatter.format(new Date(System.currentTimeMillis())));
		CreateSnapshotResult crSnapshotResult = ec2client.createSnapshot(snapshotRequest);
		Snapshot snapshot = crSnapshotResult.getSnapshot();
		SnapshotDto snapDto = new SnapshotDto(snapshot.getSnapshotId(), volumeId);
		snapshotService.addSnapshot(snapDto);
		return snapshot;
	}

	@Override
	public void deleteSnapshot(Volume associatedVolume) {
		deleteSnapshot(associatedVolume.getSnapshotId());
	}

	@Override
	public void deleteSnapshot(String snapshotId) {
		LOG.info(format("Deleting snapshot: %s", snapshotId));
		DeleteSnapshotRequest deleteSnapshotRequest = new DeleteSnapshotRequest();
		deleteSnapshotRequest.setSnapshotId(snapshotId);
		ec2client.deleteSnapshot(deleteSnapshotRequest);
		snapshotService.removeSnapshot(snapshotId);
	}
	
	@Override
	public void cleanupSnapshots(String volumeId, String snapshotIdToLeave) {
		List<SnapshotDto> snapshotsToDelete = snapshotService.getSnapshotsToDelete(volumeId, snapshotIdToLeave);
		for (SnapshotDto snapshotDto : snapshotsToDelete) {
			deleteSnapshot(snapshotDto.getSnapshotId());
		}
		
		
	}
	
	@Override
	public void cleanupSnapshots(Volume volume, Snapshot snapshotToLeave) {
		cleanupSnapshots(volume.getVolumeId(), snapshotToLeave.getSnapshotId());
	}

	@Override
	public Snapshot waitForCompleteState(Snapshot snapshot) {
		String state;
		Snapshot result;
		do {
			try {
				TimeUnit.SECONDS.sleep(20);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}

			result = syncSnapshot(snapshot);
			state = result.getState();
			if (state.equals(SnapshotState.Error)) {
				// TODO:exception
			}
		} while (state.equals(SnapshotState.Pending));

		return result;
	}

	@Override
	public Snapshot syncSnapshot(Snapshot snapshot) {
		DescribeSnapshotsRequest describeSnapshotsRequest = new DescribeSnapshotsRequest();
		LinkedList<String> ids = new LinkedList<String>();
		ids.add(snapshot.getSnapshotId());
		describeSnapshotsRequest.setSnapshotIds(ids);
		DescribeSnapshotsResult describeSnapshotsResult = ec2client.describeSnapshots(describeSnapshotsRequest);
		return describeSnapshotsResult.getSnapshots().get(0);
	}

	@Override
	public Volume waitForAvailableState(Volume volume) {
		String state;
		Volume result;
		do {
			try {
				TimeUnit.SECONDS.sleep(25);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			result = syncVolume(volume);
			state = result.getState();
			System.out.println("waitForAvailableState.current state: " + state);
			if (state.equals(VolumeState.Error.toString())) {
				throw new RuntimeException("error...");
			}
		} while (!state.equals(VolumeState.Available.toString()) && !state.equals(VolumeState.Deleted.toString()));
		return result;
	}

	@Override
	public Volume getVolume(String volumeId) {
		DescribeVolumesRequest describeVolumesRequest = new DescribeVolumesRequest();
		LinkedList<String> ids = new LinkedList<String>();
		ids.add(volumeId);
		describeVolumesRequest.setVolumeIds(ids);
		DescribeVolumesResult describeVolumesResult = ec2client.describeVolumes(describeVolumesRequest);
		return describeVolumesResult.getVolumes().get(0);
	}

	@Override
	public Instance getInstance(String instanceId) {
		DescribeInstancesRequest describeInstancesRequest = new DescribeInstancesRequest().withInstanceIds(instanceId);
		DescribeInstancesResult describeInstancesResult = ec2client.describeInstances(describeInstancesRequest);
		List<Reservation> reservations = describeInstancesResult.getReservations();
		for (Reservation res : reservations) {
			if (res.getInstances().size() > 0) {
				return res.getInstances().get(0);
			}
		}
		return null;
	}

	@Override
	public Volume createVolumeFromSnapshot(String snapshotId, String availabilityZoneName) {
		Volume vol = null;
		for (int attemptNumber = 1; attemptNumber <= retryRestoreAttempts; attemptNumber++) {
			LOG.info("Starting volume creation from {} snapshot. Attempt number - {}", snapshotId, attemptNumber);
			try {
				CreateVolumeRequest crVolumeRequest = new CreateVolumeRequest(snapshotId,
						availabilityZoneName);
				CreateVolumeResult crVolumeResult = ec2client.createVolume(crVolumeRequest);
				vol = crVolumeResult.getVolume();
				return vol;
			} catch (AmazonServiceException exception) {
				// Service error type indicates that request was valid but there was a problem at server side while processing request
				// in this case we can attempt to resend request again
				if (exception.getErrorType().equals(AmazonServiceException.ErrorType.Service) && attemptNumber != retryRestoreAttempts) {
					LOG.info("Failed to create volume from {} snapshot due to amazon service exception: {}", snapshotId, exception.getErrorMessage());
					LOG.info("New retry will occur in {} seconds", retryRestoreTimeout);
					try {
						TimeUnit.SECONDS.sleep(retryRestoreTimeout);
					} catch (InterruptedException e) {
					}
				} else {
					LOG.warn("Failed to create volume from {} snapshot due to amazon service exception: {}", snapshotId, exception.getErrorMessage());
					throw exception;
				}
			}
		}
		return vol;
	}


	@Override
	public Volume createVolumeFromSnapshot(Snapshot snapshot, String availabilityZoneName) {
		return createVolumeFromSnapshot(snapshot.getSnapshotId(), availabilityZoneName);
	}

	@Override
	public Volume syncVolume(Volume volume) {
		DescribeVolumesRequest describeVolumesRequest = new DescribeVolumesRequest().withVolumeIds(volume.getVolumeId());
		DescribeVolumesResult result = ec2client.describeVolumes(describeVolumesRequest);
		return result.getVolumes().get(0);
	}

	@Override
	public void deleteVolume(Volume volume) {

		boolean incorrectState = true;
		long timeout = 10L;
		while (incorrectState) {
			try {
				incorrectState = false;
				DeleteVolumeRequest deleteVolumeRequest = new DeleteVolumeRequest(volume.getVolumeId());
				ec2client.deleteVolume(deleteVolumeRequest);
			} catch (AmazonServiceException incorrectStateException) {
				LOG.info(incorrectStateException.getMessage() + "\n Waiting for new try");
				incorrectState = true;
				timeout += timeout < 120 ? timeout * 2 : 0;
				try {
					TimeUnit.SECONDS.sleep(timeout);
				} catch (InterruptedException e) {
				}
			}
		}
		LOG.info(format("Volume %s deleted", volume.getVolumeId()));
	}

	@Override
	public void attachVolume(Instance instance, Volume volume) {
		String deviceName = getNextAvaiableDeviceName(instance);
		boolean incorrectState = true;
		long timeout = 10L;
		while (incorrectState) {
			try {
				incorrectState = false;
				AttachVolumeRequest attachVolumeRequest = new AttachVolumeRequest(volume.getVolumeId(),
						instance.getInstanceId(), deviceName);
				AttachVolumeResult res = ec2client.attachVolume(attachVolumeRequest);
			} catch (AmazonServiceException incorrectStateException) {
				LOG.info(incorrectStateException.getMessage() + "\n Waiting for new try");
				incorrectState = true;
				timeout += timeout < 120 ? timeout * 2 : 0;
				try {
					TimeUnit.SECONDS.sleep(timeout);
				} catch (InterruptedException e) {
				}
			}
		}
		LOG.info(format("\nVolume attached. check instance data\n %s", instance.toString()));

	}

	@Override
	public void detachVolume(Volume volume) {
		boolean incorrectState = true;
		long timeout = 10L;
		while (incorrectState) {
			try {
				incorrectState = false;
				DetachVolumeRequest detachVolumeRequest = new DetachVolumeRequest(volume.getVolumeId());
				DetachVolumeResult detachVolumeResult = ec2client.detachVolume(detachVolumeRequest);
			} catch (AmazonServiceException incorrectStateException) {
				LOG.info(incorrectStateException.getMessage() + "\n Waiting for new try");
				incorrectState = true;
				timeout += timeout < 120 ? timeout * 2 : 0;
				try {
					TimeUnit.SECONDS.sleep(timeout);
				} catch (InterruptedException e) {
				}
			}
		}
		LOG.info(format("\nVolume %s unattached", volume.getVolumeId()));
	}

	@Override
	public List<Volume> getVolumeList() {
		DescribeVolumesResult volumeResult = ec2client.describeVolumes();
		return volumeResult.getVolumes();
	}


	@Override
	public List<Instance> getInstanceList() {
		DescribeInstancesResult descInstancesResult = ec2client.describeInstances();
		List<Reservation> reservations = descInstancesResult.getReservations();
		List<Instance> instances = new LinkedList<Instance>();
		for (Reservation res : reservations) {
			instances.addAll(res.getInstances());
		}
		return instances;
	}

	private String getNextAvaiableDeviceName(Instance instance) {
		String devName = "";

		List<InstanceBlockDeviceMapping> devList = instance.getBlockDeviceMappings();
		for (InstanceBlockDeviceMapping map : devList) {
			String tmp = map.getDeviceName();
			if (tmp.compareToIgnoreCase(devName) > 0) {
				devName = tmp;
			}
		}

		if (devName.length() > 0) {
			char ch = devName.charAt(devName.length() - 1);
			if (ch < 'f') {
				ch = 'f' - 1;
			}
			if (ch < 'p') {
				ch += 1;
				return "/dev/sd" + (char) ch;
			}
		}
		return "/dev/sdf";
	}

	@Override
	public void setResourceName(String resourceId, String value) {
		CreateTagsRequest r= new CreateTagsRequest().withResources(resourceId).withTags(new Tag().withKey("Name").withValue(value));
		ec2client.createTags(r);
	}

	public AmazonEC2 getEc2client() {
		return ec2client;
	}

	public void setEc2client(AmazonEC2 ec2client) {
		this.ec2client = ec2client;
	}

	public int getRetryRestoreAttempts() {
		return retryRestoreAttempts;
	}

	public void setRetryRestoreAttempts(int retryRestoreAttempts) {
		this.retryRestoreAttempts = retryRestoreAttempts;
	}

	public int getRetryRestoreTimeout() {
		return retryRestoreTimeout;
	}

	public void setRetryRestoreTimeout(int retryRestoreTimeout) {
		this.retryRestoreTimeout = retryRestoreTimeout;
	}


}

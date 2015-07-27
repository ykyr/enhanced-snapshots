package com.sangardas.snapshotdirector.tasks;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.AmazonEC2Client;
import com.amazonaws.services.ec2.model.AvailabilityZone;
import com.amazonaws.services.ec2.model.CreateSnapshotRequest;
import com.amazonaws.services.ec2.model.CreateSnapshotResult;
import com.amazonaws.services.ec2.model.CreateVolumeRequest;
import com.amazonaws.services.ec2.model.CreateVolumeResult;
import com.amazonaws.services.ec2.model.DescribeAvailabilityZonesResult;
import com.amazonaws.services.ec2.model.DescribeInstanceStatusRequest;
import com.amazonaws.services.ec2.model.DescribeInstanceStatusResult;
import com.amazonaws.services.ec2.model.DescribeInstancesRequest;
import com.amazonaws.services.ec2.model.DescribeInstancesResult;
import com.amazonaws.services.ec2.model.DescribeSnapshotsRequest;
import com.amazonaws.services.ec2.model.DescribeSnapshotsResult;
import com.amazonaws.services.ec2.model.DescribeVolumesRequest;
import com.amazonaws.services.ec2.model.DescribeVolumesResult;
import com.amazonaws.services.ec2.model.Instance;
import com.amazonaws.services.ec2.model.InstanceStatus;
import com.amazonaws.services.ec2.model.Reservation;
import com.amazonaws.services.ec2.model.Snapshot;
import com.amazonaws.services.ec2.model.Volume;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;

import static java.lang.String.format;


public class AWSBackupVolumeTask_ implements Task {

	public static final Log LOG = LogFactory.getLog(AWSBackupVolumeTask_.class);
	private AWSCredentialsProvider awsCredentialsProvider;
	private String volumeId;
	private String routineInstanceId;
	private AmazonS3 s3client;
	private AmazonEC2 ec2client;


	public AWSBackupVolumeTask_(AWSCredentialsProvider awsCredentialsProvider, String volumeId, String routineInstanceId) {
		this.awsCredentialsProvider = awsCredentialsProvider;
		this.volumeId = volumeId;
		this.routineInstanceId = routineInstanceId;
	}


	public void execute() {
		LOG.info(format("AWSBackupVolumeTask: Starting backup process for volume %s", volumeId));

		s3client = new AmazonS3Client(awsCredentialsProvider);
		ec2client = new AmazonEC2Client(awsCredentialsProvider);

		Volume volumeToBackup = findVolume(volumeId);
		Region region = getRegionForAvailabilityZone(volumeToBackup.getAvailabilityZone());
		ec2client.setRegion(region);
		LOG.info("AWSBackupVolumeTask: set client region" + region);

		Snapshot temporarySnapshot = createSnapshot(ec2client, volumeToBackup);
		Instance backupRoutineInstance = findInstance(routineInstanceId);
		InstanceStatus backupRoutineInstanceStatus = getInstanceStatus(backupRoutineInstance);

		try {
			Volume sourceTempVolume = createVolumeFromSnapshot(ec2client, temporarySnapshot,
					backupRoutineInstanceStatus.getAvailabilityZone());
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

		LOG.info(format("AWSBackupVolumeTask: Backup process for volume %s finished successfully ", volumeId));
	}


	private InstanceStatus getInstanceStatus(Instance instance) {
		DescribeInstanceStatusResult instanceStatus = ec2client
				.describeInstanceStatus(new DescribeInstanceStatusRequest().withInstanceIds(instance.getInstanceId()));
		return instanceStatus.getInstanceStatuses().get(0);
	}


	private Instance findInstance(String instanceId) {
		LOG.info("AWSBackupVolumeTask: findInstance creating new EC2 client ");
		AmazonEC2 ec2client_ = new AmazonEC2Client(awsCredentialsProvider);
		Instance inst = null;
		for (Regions rerion : Regions.values()) {
			Region nextRegion = Region.getRegion(rerion);
			ec2client_.setRegion(nextRegion);
			LOG.info("AWSBackupVolumeTask: findInstance setRegion:" + nextRegion.getName());
			try {
				DescribeInstancesResult res = ec2client_.describeInstances(new DescribeInstancesRequest()
						.withInstanceIds(instanceId));
				List<Reservation> reservations = res.getReservations();
				for (Reservation reservation : reservations) {
					if (reservation.getInstances().size() > 0) {
						inst = reservation.getInstances().get(0);
						LOG.info(format("AWSBackupVolumeTask: finded routine instance %s", inst));
						break;
					}
				}
				if(inst!=null) break;
			} catch (AmazonServiceException exception) {
				if (exception.getStatusCode() == 401) {
					LOG.info("AWSBackupVolumeTask: findInstance: not autorized!");
				}
			}
		}

		if (inst == null) {
			String errorMessage = format("AWSBackupVolumeTask: Can't find routine instance %s", instanceId);
			LOG.error(errorMessage);
			throw new RuntimeException(errorMessage);
		}
		return inst;

	}


	private Volume findVolume(String volumeId) {
		LOG.info("AWSBackupVolumeTask: findVolume creating new EC2 client ");
		AmazonEC2 ec2client_ = new AmazonEC2Client(awsCredentialsProvider);
		Volume vol = null;
		for (Regions r : Regions.values()) {
			Region nextRegion = Region.getRegion(r);
			ec2client_.setRegion(nextRegion);
			LOG.info("AWSBackupVolumeTask: findVolume setRegion:" + nextRegion.getName());
			try {
				DescribeVolumesResult res = ec2client_.describeVolumes(new DescribeVolumesRequest());
				if (res.getVolumes().size() > 0) {
					vol = res.getVolumes().get(0);
					LOG.info(format("AWSBackupVolumeTask: finded volume to backup %s", vol));
					break;
				}
			} catch (AmazonServiceException exception) {
				if (exception.getStatusCode() == 401) {
					LOG.info("AWSBackupVolumeTask: findVolume: not autorized!");
				}
			}
		}

		if (vol == null) {
			LOG.info(format("AWSBackupVolumeTask: Can't find volume to backup with id %s", volumeId));
		}
		return vol;
	}


	private Region getRegionForAvailabilityZone(String availabilityZone) {
		return Region.getRegion(Regions.fromName(availabilityZone.substring(0, availabilityZone.length() - 1)));
	}


	public Snapshot createSnapshot(AmazonEC2 ec2client, Volume volume) {
		SimpleDateFormat formatter = new SimpleDateFormat("yyyy.MM.dd'_T'hh:mm:ss");
		String volumeId = volume.getVolumeId();
		LOG.info(format("AWSBackupVolumeTask: creating snapshot for volume %s", volumeId));

		CreateSnapshotRequest snapshotRequest = new CreateSnapshotRequest(volumeId, volumeId + "__"
				+ formatter.format(new Date(System.currentTimeMillis())));
		CreateSnapshotResult crSnapshotResult = ec2client.createSnapshot(snapshotRequest);
		Snapshot snapshot = crSnapshotResult.getSnapshot();
		LOG.info(format("AWSBackupVolumeTask: snapshot description %s", snapshot));
		return snapshot;
	}


	public Volume createVolumeFromSnapshot(AmazonEC2 ec2client, Snapshot sourceSnapshot, String availabilityZoneName)
			throws InterruptedException {

		Volume vol = null;
		LOG.info(format("AWSBackupVolumeTask: creating volume from snapshot %s", sourceSnapshot.getSnapshotId()));

		CreateVolumeRequest crVolumeRequest = new CreateVolumeRequest(sourceSnapshot.getSnapshotId(),
				availabilityZoneName);
		int secondsToWait = 30;
		int triesCounter = 0;
		while (triesCounter < 20) {
			try {
				CreateVolumeResult crVolumeResult = ec2client.createVolume(crVolumeRequest);
			} catch (AmazonServiceException snapshotUnavailable) {
				LOG.info(format("AWSBackupVolumeTask: can't access snapshot  %s", sourceSnapshot.getSnapshotId()));
				LOG.info(format("AWSBackupVolumeTask: waiting for next try after  %d seconds", secondsToWait));
				triesCounter++;
				secondsToWait = secondsToWait + secondsToWait / 3;
				TimeUnit.SECONDS.sleep(secondsToWait);
			}
		}

		CreateVolumeResult crVolumeResult = ec2client.createVolume(crVolumeRequest);
		vol = crVolumeResult.getVolume();
		LOG.info(format("AWSBackupVolumeTask: volume description %s", vol));
		return vol;
	}

}

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


public class AWSBackupVolumeTask implements Task {
	//TODO: remove temporary counter, use dbdata instead
	public static int taskId = 0;
	
	public static final Log LOG = LogFactory.getLog(AWSBackupVolumeTask.class);
	private AWSCredentialsProvider awsCredentialsProvider;
	private String volumeId;
	private String routineInstanceId;


	public AWSBackupVolumeTask(AWSCredentialsProvider awsCredentialsProvider, String volumeId, String routineInstanceId) {
		this.awsCredentialsProvider = awsCredentialsProvider;
		this.volumeId = volumeId;
		this.routineInstanceId = routineInstanceId;
	}


	public void execute() {
		LOG.info(format("AWSBackupVolumeTask[%d]: Starting backup process for volume %s",taskId, volumeId));


		// TODO: create snapshot, create temporary volume, attach volume, backup, detach tempvolume
		// TODO: sdfs backup
		// TODO: detach temporary volume, delete temporary volume, delete snapshot

		LOG.info(format("AWSBackupVolumeTask[%d]: Backup process for volume %s finished successfully ",taskId, volumeId));
	}

}

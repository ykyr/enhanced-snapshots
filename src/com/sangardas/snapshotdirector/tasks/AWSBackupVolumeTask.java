package com.sangardas.snapshotdirector.tasks;

import static java.lang.String.format;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.amazonaws.auth.AWSCredentialsProvider;


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

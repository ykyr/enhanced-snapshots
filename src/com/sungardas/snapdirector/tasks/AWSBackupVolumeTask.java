package com.sungardas.snapdirector.tasks;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.AmazonEC2Client;
import com.amazonaws.services.ec2.model.Volume;
import com.sungardas.snapdirector.aws.dynamodb.DynamoUtils;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupState;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.aws.dynamodb.repository.BackupRepository;
import com.sungardas.snapdirector.aws.dynamodb.repository.TaskRepository;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.tasks.aws.VolumeBackup;
import com.sungardas.snapdirector.tasks.aws.sdfs.utils.SdfsManager;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.io.IOException;

import static java.lang.String.format;


public class AWSBackupVolumeTask implements Task {
	private static final Logger LOG = LogManager.getLogger(AWSBackupVolumeTask.class);
	@Autowired
	private TaskRepository taskRepository;

	@Autowired
	private AWSCredentials amazonAWSCredentials;

	@Autowired
	AmazonEC2 ec2client;

	@Autowired
	BackupRepository backupRepository;

	private TaskEntry taskEntry;

	@Autowired
	private ConfigurationService configurationService;

	private WorkerConfiguration configuration;

	public void setTaskEntry(TaskEntry taskEntry) {
		this.taskEntry = taskEntry;
	}

	public void execute() {
		String volumeId = taskEntry.getVolume();
		configuration = configurationService.getConfiguration();

		LOG.info(format("AWSBackupVolumeTask: Starting backup process for volume %s", volumeId));
		LOG.info("Task " + taskEntry.getId() + ": Change task state to 'inprogress'");
        taskEntry.setStatus("running");
        taskRepository.save(taskEntry);
		
		SdfsManager sdfs = new SdfsManager(configuration);

		Volume tempVolume = null;
		String attachedDeviceName = null;
		if (!configuration.isUseFakeEC2()) {
			tempVolume = VolumeBackup.createAndAttachBackupVolume(ec2client, volumeId,
					configuration.getConfigurationId());
			attachedDeviceName = tempVolume.getAttachments().get(0).getDevice();
		}

		String backupDate = String.valueOf(System.currentTimeMillis());
		String backupfileName = volumeId + "." + backupDate + ".backup";

		BackupEntry backup = new BackupEntry(volumeId, backupfileName, backupDate, "", BackupState.INPROGRESS,
				configuration.getConfigurationId());
		backupRepository.save(backup);

		boolean backupStatus = false;
		try {
			if (configuration.isUseFakeBackup()) {
				backupStatus = sdfs.backupVolumeToSdfs(configuration.getFakeBackupSource(), backupfileName);
			} else if (!configuration.isUseFakeEC2()) {
				backupStatus = sdfs.backupVolumeToSdfs(attachedDeviceName, backupfileName);

			}
		} catch (IOException e) {
			LOG.fatal(format("Backup of volume %s failed", volumeId));
			backup.setState(BackupState.FAILED.getState());
			backupRepository.save(backup);
		}

		if (backupStatus) {
			long backupSize = sdfs.getBackupSize(backupfileName);
			LOG.info("Backup creation time: " + sdfs.getBackupCreationTime(backupfileName));
			LOG.info("Backup size: " + backupSize);

			LOG.info("Put backup entry to the Backup List: " + backup.toString());
			backup.setState(BackupState.COMPLETED.getState());
			backup.setSize(String.valueOf(backupSize));
			backupRepository.save(backup);
		}

		if (!configuration.isUseFakeEC2()) {
			VolumeBackup.detachAndDeleteVolume(ec2client, tempVolume);
		}

		LOG.info(format("Backup process for volume %s finished successfully ", volumeId));
		LOG.info("Task " + taskEntry.getId() + ": Delete completed task:" + taskEntry.getId());
        taskRepository.delete(taskEntry);
        LOG.info("Task completed.");
	}

}

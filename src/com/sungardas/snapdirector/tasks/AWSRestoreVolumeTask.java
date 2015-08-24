package com.sungardas.snapdirector.tasks;

import static java.lang.String.format;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.TimeUnit;

import com.sungardas.snapdirector.exception.DataAccessException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.services.ec2.model.Instance;
import com.amazonaws.services.ec2.model.Volume;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.aws.dynamodb.repository.BackupRepository;
import com.sungardas.snapdirector.aws.dynamodb.repository.TaskRepository;
import com.sungardas.snapdirector.service.AWSCommunticationService;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.StorageService;

import org.springframework.context.annotation.Profile;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope("prototype")
@Profile("prod")
public class AWSRestoreVolumeTask implements RestoreTask {
	private static final Logger LOG = LogManager.getLogger(AWSRestoreVolumeTask.class);

	@Autowired
	private TaskRepository taskRepository;
	@Autowired
	private BackupRepository backupRepository;

	@Autowired
	private AWSCredentials amazonAWSCredentials;

	@Autowired
	private AWSCommunticationService awsCommunication;

	@Autowired
	private StorageService storageService;

	private TaskEntry taskEntry;

	@Autowired
	private ConfigurationService configurationService;

	private WorkerConfiguration configuration;

	@Override
	public void setTaskEntry(TaskEntry taskEntry) {
		this.taskEntry = taskEntry;

	}

	@Override
	public void execute() {
		String sourceFile = taskEntry.getOptions();
		configuration = configurationService.getConfiguration();
		changeTaskStatusToRunning();
		try {
			if (sourceFile == null || sourceFile.isEmpty()) {
				restoreFromSnapshot();
			} else {
				restoreFromBackupFile();
			}
			deleteCompletedTask();
		} catch (RuntimeException e) {
			LOG.error("Failed to execute {} task {}. Changing task status to '{}'", taskEntry.getType(), taskEntry.getId(), TaskEntry.TaskEntryStatus.ERROR);
			taskEntry.setStatus(TaskEntry.TaskEntryStatus.ERROR.getStatus());
			taskRepository.save(taskEntry);
		}
	}

	private void changeTaskStatusToRunning() {
		LOG.info("Status of {} task {} was changed to '{}'", taskEntry.getType(), taskEntry.getId(), TaskEntry.TaskEntryStatus.RUNNING);
		taskEntry.setStatus(TaskEntry.TaskEntryStatus.RUNNING.getStatus());
		taskRepository.save(taskEntry);
	}

	private void deleteCompletedTask() {
		LOG.info("Deleting completed {} task {}", taskEntry.getType(), taskEntry.getId());
		taskRepository.delete(taskEntry);
		LOG.info("{} task {} was completed and removed", taskEntry.getType(), taskEntry.getId());
	}

	private void restoreFromSnapshot() {
		String volumeId = taskEntry.getVolume();
		BackupEntry backupEntry = backupRepository.getLast(volumeId);
		if (backupEntry == null) {
			LOG.error("Failed to find backup entry for volume {} ", volumeId);
			throw new DataAccessException("Backup for volume: " + volumeId + " was not found");
		}
		awsCommunication.createVolumeFromSnapshot(backupEntry.getSnapshotId(), awsCommunication.getVolume(volumeId).getAvailabilityZone());
	}

	private void restoreFromBackupFile() {
		String volumeId = taskEntry.getVolume();
		String sourceFile = taskEntry.getOptions();
		String instanceId = taskEntry.getInstanceId();

		BackupEntry backupentry = backupRepository.getByBackupFileName(sourceFile);
		Instance instance = awsCommunication.getInstance(instanceId);
		String volumeType = backupentry.getVolumeType();
		String size = backupentry.getSizeGiB();
		String iops = backupentry.getIops();
		Volume volumeToRestore = null;
		switch (volumeType) {
			case "standard":
				volumeToRestore = awsCommunication.createStandardVolume(Integer.parseInt(size));
				break;
			case "gp2":
				volumeToRestore = awsCommunication.createGP2Volume(Integer.parseInt(size));
				break;
			case "io1":
				volumeToRestore = awsCommunication.createIO1Volume(Integer.parseInt(size), Integer.parseInt(iops));
				break;
		}

		awsCommunication.attachVolume(instance, volumeToRestore);
		
		//wait for attached state
		
		while (volumeToRestore.getAttachments().size() == 0) {
			sleep();
			volumeToRestore = awsCommunication.syncVolume(volumeToRestore);
		}
		
		String attachedDeviceName = storageService.detectFsDevName(volumeToRestore);
		
		try {
			storageService.copyFile(configuration.getSdfsMountPoint() + backupentry.getFileName(), attachedDeviceName);
		} catch (IOException e) {
			e.printStackTrace();
		}

		awsCommunication.detachVolume(volumeToRestore);

	}

	private void sleep() {
		try {
			TimeUnit.SECONDS.sleep(10);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}

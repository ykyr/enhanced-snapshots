package com.sungardas.snapdirector.tasks;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.services.ec2.model.Instance;
import com.amazonaws.services.ec2.model.Volume;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.BackupRepository;
import com.sungardas.snapdirector.aws.dynamodb.repository.TaskRepository;
import com.sungardas.snapdirector.service.AWSCommunticationService;
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

	private TaskEntry taskEntry;

	@Override
	public void setTaskEntry(TaskEntry taskEntry) {
		this.taskEntry = taskEntry;

	}

	@Override
	public void execute() {
		String sourceFile = taskEntry.getOptions();

		changeTaskStatusToRunning();

		if (sourceFile == null) {
			restoreFromSnapshot();
		} else {
			restoreFromBackupFile();
		}

		deleteCompletedTask();
	}
	
	private void changeTaskStatusToRunning() {
		LOG.info("Task " + taskEntry.getId() + ": Change task state to 'inprogress'");
		taskEntry.setStatus("running");
		taskRepository.save(taskEntry);
	}

	private void deleteCompletedTask() {
		LOG.info("Task " + taskEntry.getId() + ": Delete completed task:" + taskEntry.getId());
		taskRepository.delete(taskEntry);
		LOG.info("Task completed.");
	}

	private void restoreFromSnapshot() {

	}

	private void restoreFromBackupFile() {
		String volumeId = taskEntry.getVolume();
		String sourceFile = taskEntry.getOptions();
		String instanceId = taskEntry.getInstanceId();
		
		BackupEntry backupentry = backupRepository.getLast(volumeId);
		Instance instance = awsCommunication.getInstance(instanceId);
		String volumeType = backupentry.getVolumeType();
		String size =backupentry.getSizeGiB();
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
		
		//TODO: binary copy from backup to volume
		
		awsCommunication.detachVolume(volumeToRestore);
		
	}

	

}

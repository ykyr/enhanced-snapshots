package com.sungardas.snapdirector.tasks;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import com.amazonaws.auth.AWSCredentials;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.BackupRepository;
import com.sungardas.snapdirector.aws.dynamodb.repository.TaskRepository;
import com.sungardas.snapdirector.service.AWSCommunticationService;

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

	private void restoreFromSnapshot() {

	}

	private void restoreFromBackupFile() {

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

}

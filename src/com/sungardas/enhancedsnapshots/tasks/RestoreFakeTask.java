package com.sungardas.enhancedsnapshots.tasks;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.TaskEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.TaskRepository;
import com.sungardas.enhancedsnapshots.service.NotificationService;
import com.sungardas.enhancedsnapshots.service.TaskService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

@Component
@Scope("prototype")
@Profile("dev")
public class RestoreFakeTask implements RestoreTask {
	private static final Logger LOG = LogManager.getLogger(RestoreFakeTask.class);

	@Autowired
	private TaskRepository taskRepository;

	@Autowired
	private NotificationService notificationService;

	@Autowired
	private TaskService taskService;

	private TaskEntry taskEntry;

	@Override
	public void setTaskEntry(TaskEntry taskEntry) {
		this.taskEntry = taskEntry;

	}

	@Override
	public void execute() {
		notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Starting restore...", 0);
		LOG.info("Task " + taskEntry.getId() + ": Change task state to 'inprogress'");
		taskEntry.setStatus("running");
		taskRepository.save(taskEntry);
		String[] options = taskEntry.getOptions().split(", ");
		String targetZone = options[1];

		String sourceFile = options[0];
		LOG.info("restore from: {}; restore to az: {}", sourceFile, targetZone);
		String instanceId = taskEntry.getInstanceId();
		notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Restoring...", 50);

		try {
			TimeUnit.SECONDS.sleep(10);
		} catch (InterruptedException ignored) {
		}

		LOG.info("Task " + taskEntry.getId() + ": Delete completed task:" + taskEntry.getId());
		taskService.complete(taskEntry);
		LOG.info("Task completed.");
		notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Task complete", 100);

	}

}

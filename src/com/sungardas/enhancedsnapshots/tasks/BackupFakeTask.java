package com.sungardas.enhancedsnapshots.tasks;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.BackupEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.BackupState;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.TaskEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.BackupRepository;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.TaskRepository;
import com.sungardas.enhancedsnapshots.dto.CopyingTaskProgressDto;
import com.sungardas.enhancedsnapshots.service.NotificationService;
import com.sungardas.enhancedsnapshots.service.RetentionService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

import static com.sungardas.enhancedsnapshots.aws.dynamodb.model.TaskEntry.TaskEntryStatus.RUNNING;

@Component
@Scope("prototype")
@Profile("dev")
public class BackupFakeTask implements BackupTask {
	private static final Logger LOG = LogManager.getLogger(BackupFakeTask.class);
    
    @Autowired
	private TaskRepository taskRepository;
    @Autowired
	private BackupRepository backupRepository;
    
    @Autowired
    private RetentionService retentionService;

    @Autowired
    private NotificationService notificationService;

    private TaskEntry taskEntry;

    
    public void setTaskEntry(TaskEntry taskEntry) {
    	this.taskEntry= taskEntry;
    }

    @Override
    public void execute() {
        LOG.info("Task " + taskEntry.getId() + ": Change task state to 'inprogress'");
        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Starting delete task", 0);

        taskEntry.setStatus(RUNNING.getStatus());
        taskRepository.save(taskEntry);

        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Finding source file", 30);
        LOG.info(taskEntry.toString());
        String timestamp = Long.toString(System.currentTimeMillis());
        String volumeId = taskEntry.getVolume();
        String filename = volumeId + "." + timestamp + ".backup";
        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Checking volume", 60);
        BackupEntry backup = new BackupEntry(taskEntry.getVolume(), filename, timestamp, "123456789", BackupState.COMPLETED, taskEntry.getInstanceId(),
        		"snap-00100110","gp2","3000", "10");
        LOG.info("Task " + taskEntry.getId() + ":put backup info'");
        backupRepository.save(backup);

        CopyingTaskProgressDto dto = new CopyingTaskProgressDto(taskEntry.getId(), 60, 100, 1);
        for (int i = 0; i <= 10; i++) {
            try {
                TimeUnit.SECONDS.sleep(1);
            } catch (InterruptedException ignored) {
            }
            dto.setCopyingProgress(i * 100000000);
            notificationService.notifyAboutTaskProgress(dto);
        }

        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Task complete", 100);
        LOG.info("Task " + taskEntry.getId() + ": Delete completed task:" + taskEntry.getId());
        taskRepository.delete(taskEntry);
        LOG.info("Task completed.");
        retentionService.apply();
    }

}

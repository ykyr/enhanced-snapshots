package com.sungardas.enhancedsnapshots.service.impl;

import com.amazonaws.services.ec2.model.VolumeType;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.BackupEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.SnapshotEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.TaskEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.BackupRepository;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.SnapshotRepository;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.TaskRepository;
import com.sungardas.enhancedsnapshots.dto.ExceptionDto;
import com.sungardas.enhancedsnapshots.dto.TaskDto;
import com.sungardas.enhancedsnapshots.dto.converter.TaskDtoConverter;
import com.sungardas.enhancedsnapshots.exception.DataAccessException;
import com.sungardas.enhancedsnapshots.exception.EnhancedSnapshotsException;
import com.sungardas.enhancedsnapshots.service.*;
import com.sungardas.enhancedsnapshots.tasks.AWSRestoreVolumeTask;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.*;

@Service
public class TaskServiceImpl implements TaskService {

    private static final Logger LOG = LogManager.getLogger(TaskServiceImpl.class);
    private static final long TTL = 300000;

    @Autowired
    private TaskRepository taskRepository;

    @Autowired
    private BackupRepository backupRepository;

    @Autowired
    private SnapshotRepository snapshotRepository;

    @Value("${sungardas.worker.configuration}")
    private String configurationId;

    @Value("${enhancedsnapshots.queue.size}")
    private int queueSize;

    @Autowired
    private ConfigurationService configuration;

    @Autowired
    private SchedulerService schedulerService;

    @Autowired
    private NotificationService notificationService;

    @PostConstruct
    private void init() {
        schedulerService.addTask(new Task() {
            @Override
            public String getId() {
                return "taskRetentionPolicy";
            }

            @Override
            public void run() {
                long currentTime = System.currentTimeMillis();
                List<TaskEntry> list = taskRepository.findByExpirationDateLessThanEqualAndInstanceId(currentTime + "", configurationId);
                taskRepository.delete(list);
            }
        }, "*/5 * * * *");

        // In case of migration from one ES version to another there can be regular tasks
        // without tempVolumeType and tempVolumeIops properties. Here we are going to set them with default values
        // TODO: move this code to migration logic when implementing SNAP-338
        List<TaskEntry> regularTasks = taskRepository.findByRegular(Boolean.TRUE.toString());
        for (TaskEntry task : regularTasks) {
            if (task.getTempVolumeType() == null) {
                task.setTempVolumeType(configuration.getConfiguration().getTempVolumeType());
                task.setTempVolumeIopsPerGb(configuration.getConfiguration().getTempVolumeIopsPerGb());
            }
        }
    }

    @Override
    public Map<String, String> createTask(TaskDto taskDto) {
        Map<String, String> messages = new HashMap<>();
        String configurationId = configuration.getConfiguration().getConfigurationId();
        List<TaskEntry> validTasks = new ArrayList<>();
        int tasksInQueue = getTasksInQueue();
        boolean regular = Boolean.valueOf(taskDto.getRegular());
        for (TaskEntry taskEntry : TaskDtoConverter.convert(taskDto)) {
            if (!regular && tasksInQueue >= queueSize) {
                notificationService.notifyAboutError(new ExceptionDto("Task creation error", "Task queue is full"));
                break;
            }
            taskEntry.setWorker(configurationId);
            taskEntry.setInstanceId(configurationId);
            taskEntry.setStatus(TaskEntry.TaskEntryStatus.QUEUED.getStatus());
            taskEntry.setId(UUID.randomUUID().toString());

            // set tempVolumeType and iops if required
            setTempVolumeAndIops(taskEntry);

            if (regular) {
                try {
                    schedulerService.addTask(taskEntry);
                    messages.put(taskEntry.getVolume(), getMessage(taskEntry));
                    validTasks.add(taskEntry);
                    tasksInQueue++;
                } catch (EnhancedSnapshotsException e) {
                    notificationService.notifyAboutError(new ExceptionDto("Task creation has failed", e.getLocalizedMessage()));
                    LOG.error(e);
                    messages.put(taskEntry.getVolume(), e.getLocalizedMessage());
                }
            } else if (TaskEntry.TaskEntryType.RESTORE.getType().equals(taskEntry.getType())) {
                if (backupRepository.getLast(taskEntry.getVolume(), configurationId) == null || snapshotRepository.findOne(SnapshotEntry.getId(taskEntry.getVolume(), configurationId)) == null) {
                    notificationService.notifyAboutError(new ExceptionDto("Restore task error", "Backup for volume: " + taskEntry.getVolume() + " not found!"));
                    messages.put(taskEntry.getVolume(), "Restore task error");
                } else {
                    setRestoreVolumeTypeAndIops(taskEntry);
                    messages.put(taskEntry.getVolume(), getMessage(taskEntry));
                    validTasks.add(taskEntry);
                    tasksInQueue++;
                }
            } else {
                messages.put(taskEntry.getVolume(), getMessage(taskEntry));
                validTasks.add(taskEntry);
                tasksInQueue++;
            }

        }
        taskRepository.save(validTasks);
        return messages;
    }

    private String getMessage(TaskEntry taskEntry) {
        switch (taskEntry.getType()) {
            case "restore": {
                BackupEntry backupEntry = null;
                String sourceFile = taskEntry.getSourceFileName();
                if (sourceFile == null || sourceFile.isEmpty()) {
                    backupEntry = backupRepository.getLast(taskEntry.getVolume(), configurationId);

                } else {
                    backupEntry = backupRepository.getByBackupFileName(sourceFile);
                }
                if (backupEntry == null) {
                    //TODO: add more messages
                    return "Unable to execute: backup history is empty";
                } else {
                    return AWSRestoreVolumeTask.RESTORED_NAME_PREFIX + backupEntry.getFileName();
                }
            }
        }
        return "Processed";
    }

    @Override
    public List<TaskDto> getAllTasks() {
        try {
            return TaskDtoConverter.convert(taskRepository.findByRegularAndInstanceId(Boolean.FALSE.toString(),
                    configuration.getConfiguration().getConfigurationId()));
        } catch (RuntimeException e) {
            notificationService.notifyAboutError(new ExceptionDto("Getting tasks have failed", "Failed to get tasks."));
            LOG.error("Failed to get tasks.", e);
            throw new DataAccessException("Failed to get tasks.", e);
        }
    }

    @Override
    public List<TaskDto> getAllTasks(String volumeId) {
        try {
            return TaskDtoConverter.convert(taskRepository.findByRegularAndVolumeAndInstanceId(Boolean.FALSE.toString(),
                    volumeId, configuration.getConfiguration().getConfigurationId()));
        } catch (RuntimeException e) {
            notificationService.notifyAboutError(new ExceptionDto("Getting tasks have failed", "Failed to get tasks."));
            LOG.error("Failed to get tasks.", e);
            throw new DataAccessException("Failed to get tasks.", e);
        }
    }

    @Override
    public void complete(TaskEntry taskEntry) {
        long expirationDate = System.currentTimeMillis() + TTL;
        taskEntry.setExpirationDate(expirationDate + "");
        taskEntry.setStatus(TaskEntry.TaskEntryStatus.COMPLETE.getStatus());
        taskRepository.save(taskEntry);
    }

    @Override
    public boolean isQueueFull() {
        return getTasksInQueue() > queueSize;
    }

    @Override
    public List<TaskDto> getAllRegularTasks(String volumeId) {
        try {
            return TaskDtoConverter.convert(taskRepository.findByRegularAndVolumeAndInstanceId(Boolean.TRUE.toString(),
                    volumeId, configuration.getConfiguration().getConfigurationId()));
        } catch (RuntimeException e) {
            notificationService.notifyAboutError(new ExceptionDto("Getting tasks have failed", "Failed to get tasks."));
            LOG.error("Failed to get tasks.", e);
            throw new DataAccessException("Failed to get tasks.", e);
        }
    }

    @Override
    public void removeTask(String id) {
        if (taskRepository.exists(id)) {
            TaskEntry taskEntry = taskRepository.findOne(id);
            if (TaskEntry.TaskEntryStatus.RUNNING.getStatus().equals(taskEntry.getStatus())) {
                throw new EnhancedSnapshotsException("Can`t remove task " + id + ", task in status: " + taskEntry.getStatus());
            }
            taskRepository.delete(id);
            if (Boolean.valueOf(taskEntry.getRegular())) {
                schedulerService.removeTask(taskEntry.getId());
            }
            LOG.info("TaskEntry {} was removed successfully.", id);
        } else {
            LOG.info("TaskEntry {} can not be removed since it does not exist.", id);
        }
    }

    @Override
    public boolean isCanceled(String id) {
        return !taskRepository.exists(id);
    }

    @Override
    public void updateTask(TaskDto taskInfo) {
        removeTask(taskInfo.getId());
        createTask(taskInfo);
    }

    private int getTasksInQueue() {
        return (int) (taskRepository.countByRegularAndInstanceIdAndTypeAndStatus(Boolean.FALSE.toString(), configurationId, TaskEntry.TaskEntryType.BACKUP.getType(), TaskEntry.TaskEntryStatus.QUEUED.getStatus()) +
                taskRepository.countByRegularAndInstanceIdAndTypeAndStatus(Boolean.FALSE.toString(), configurationId, TaskEntry.TaskEntryType.RESTORE.getType(), TaskEntry.TaskEntryStatus.QUEUED.getStatus()));
    }

    // for restore and backup tasks we should specify temp volume type
    private void setTempVolumeAndIops(TaskEntry taskEntry){
        if(taskEntry.getType().equals(TaskEntry.TaskEntryType.RESTORE.getType()) || taskEntry.getType().equals(TaskEntry.TaskEntryType.BACKUP.getType())){
            taskEntry.setTempVolumeType(configuration.getSystemConfiguration().getSystemProperties().getTempVolumeType());
            if(configuration.getSystemConfiguration().getSystemProperties().getTempVolumeType().equals(VolumeType.Io1.toString())){
                taskEntry.setTempVolumeIopsPerGb(configuration.getSystemConfiguration().getSystemProperties().getTempVolumeIopsPerGb());
            }
        }
    }

    // for restore tasks we should specify restore volume type
    private void setRestoreVolumeTypeAndIops(TaskEntry taskEntry){
        if(taskEntry.getType().equals(TaskEntry.TaskEntryType.RESTORE.getType())){
            taskEntry.setRestoreVolumeType(configuration.getSystemConfiguration().getSystemProperties().getRestoreVolumeType());
            if(configuration.getSystemConfiguration().getSystemProperties().getRestoreVolumeType().equals(VolumeType.Io1.toString())){
                taskEntry.setRestoreVolumeIopsPerGb(configuration.getSystemConfiguration().getSystemProperties().getRestoreVolumeIopsPerGb());
            }
        }
    }
}

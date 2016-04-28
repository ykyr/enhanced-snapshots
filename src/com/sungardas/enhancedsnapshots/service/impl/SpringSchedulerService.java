package com.sungardas.enhancedsnapshots.service.impl;

import com.amazonaws.AmazonClientException;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.TaskEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.TaskRepository;
import com.sungardas.enhancedsnapshots.dto.ExceptionDto;
import com.sungardas.enhancedsnapshots.exception.EnhancedSnapshotsException;
import com.sungardas.enhancedsnapshots.service.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.DependsOn;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.*;
import java.util.concurrent.ScheduledFuture;

@Service
@DependsOn("CreateAppConfiguration")
public class SpringSchedulerService implements SchedulerService {

    private static final Logger LOG = LogManager.getLogger(SpringSchedulerService.class);

    @Qualifier("taskScheduler")
    @Autowired
    private TaskScheduler scheduler;

    @Autowired
    private ConfigurationService configurationService;

    @Autowired
    private TaskRepository taskRepository;

    private Map<String, ScheduledFuture> jobs = new HashMap<>();

    private String instanceId;

    @Autowired
    private VolumeService volumeService;

    @Autowired
    private NotificationService notificationService;

    @Autowired
    private TaskService taskService;

    @PostConstruct
    private void init() {
        try {
            instanceId = configurationService.getConfiguration().getConfigurationId();
            List<TaskEntry> tasks = taskRepository.findByRegularAndInstanceId(Boolean.TRUE.toString(), instanceId);
            for (TaskEntry taskEntry : tasks) {
                try {
                    addTask(taskEntry);
                } catch (EnhancedSnapshotsException e) {
                    LOG.error(e);
                }
            }
        } catch (AmazonClientException e) {
            LOG.error(e);
        }
    }

    @Override
    public void addTask(TaskEntry taskEntry) {
        if (TaskEntry.TaskEntryType.BACKUP.getType().equals(taskEntry.getType()) && taskEntry.getCron() != null && !taskEntry.getCron().isEmpty()) {
            if (Boolean.valueOf(taskEntry.getEnabled())) {
                addTask(new TaskImpl(taskEntry), taskEntry.getCron());
            }
            volumeService.expireCache();
        } else {
            throw new EnhancedSnapshotsException("Invalid task: " + taskEntry);
        }
    }

    @Override
    public void addTask(Task task, String cronExpression) {
        ScheduledFuture<?> future = scheduler.schedule(task, new CronTrigger("0 " + cronExpression));
        jobs.put(task.getId(), future);
    }

    @Override
    public void removeTask(String id) {
        ScheduledFuture future = jobs.remove(id);
        if (future != null) {
            future.cancel(false);
            volumeService.expireCache();
        } else {
            LOG.debug("Task with id: {} not found", id);
        }
    }

    @Override
    public Set<String> getVolumeIdsWithSchedule() {
        Set<String> result = new HashSet<>();
        for (TaskEntry taskEntry : taskRepository.findByRegularAndInstanceIdAndEnabled(Boolean.TRUE.toString(), instanceId, Boolean.TRUE.toString())) {
            result.add(taskEntry.getVolume());
        }
        return result;
    }

    private class TaskImpl implements Task {

        private TaskEntry taskEntry;

        public TaskImpl(TaskEntry taskEntry) {
            this.taskEntry = taskEntry;
        }

        @Override
        public void run() {
            if (taskService.isQueueFull()) {
                notificationService.notifyAboutError(new ExceptionDto("Task creation error", "Task queue is full"));
            } else {
                taskEntry.setId(UUID.randomUUID().toString());
                taskEntry.setSchedulerManual(false);
                taskEntry.setRegular(false);
                taskEntry.setSchedulerTime(String.valueOf(DateTime.now().getMillis()));
                taskRepository.save(taskEntry);
            }
        }

        @Override
        public String getId() {
            return taskEntry.getId();
        }

    }
}

package com.sungardas.enhancedsnapshots.service.impl;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ScheduledFuture;

import javax.annotation.PostConstruct;

import com.amazonaws.AmazonClientException;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.TaskEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.TaskRepository;
import com.sungardas.enhancedsnapshots.exception.EnhancedSnapshotsException;
import com.sungardas.enhancedsnapshots.service.ConfigurationService;
import com.sungardas.enhancedsnapshots.service.SchedulerService;
import com.sungardas.enhancedsnapshots.service.Task;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.DependsOn;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.stereotype.Service;

@Service
@DependsOn("CreateAppConfiguration")
public class SpringSchedulerService implements SchedulerService {

    private static final Logger LOG = LogManager.getLogger(SpringSchedulerService.class);

    @Autowired
    private TaskScheduler scheduler;

    @Autowired
    private ConfigurationService configurationService;

    @Autowired
    private TaskRepository taskRepository;

    private Map<String, ScheduledFuture> jobs = new HashMap<>();

    private String instanceId;

    @PostConstruct
    private void init() {
        try {
            instanceId = configurationService.getWorkerConfiguration().getConfigurationId();
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
            taskEntry.setId(null);
            taskEntry.setSchedulerManual(false);
            taskEntry.setRegular(false);
            taskEntry.setSchedulerTime(String.valueOf(DateTime.now().getMillis()));
            taskRepository.save(taskEntry);
        }

        @Override
        public String getId() {
            return taskEntry.getId();
        }

    }
}

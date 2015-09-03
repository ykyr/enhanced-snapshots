package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.TaskRepository;
import com.sungardas.snapdirector.exception.SnapdirectorException;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.SchedulerService;
import com.sungardas.snapdirector.service.Task;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledFuture;

@Service
public class SpringSchedulerService implements SchedulerService {

    private static final Logger LOG = LogManager.getLogger(SpringSchedulerService.class);

    @Autowired
    private TaskScheduler scheduler;

    @Autowired
    private ConfigurationService configurationService;

    @Autowired
    private TaskRepository taskRepository;

    private Map<String, ScheduledFuture> jobs = new HashMap<>();

    @PostConstruct
    private void init() {
        List<TaskEntry> tasks = taskRepository.findByInstanceIdAndRegular(configurationService.getConfiguration().getConfigurationId(), Boolean.TRUE.toString());
        for (TaskEntry taskEntry : tasks) {
            try {
                addTask(taskEntry);
            } catch (SnapdirectorException e) {
                LOG.error(e);
            }
        }
    }

    @Override
    public void addTask(TaskEntry taskEntry) {
        if (TaskEntry.TaskEntryType.BACKUP.getType().equals(taskEntry.getType()) && taskEntry.getCron() != null && !taskEntry.getCron().isEmpty()) {
            if (Boolean.valueOf(taskEntry.getEnabled())) {
                addTask(new TaskImpl(taskEntry), taskEntry.getCron());
            }
        } else {
            throw new SnapdirectorException("Invalid task: " + taskEntry);
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

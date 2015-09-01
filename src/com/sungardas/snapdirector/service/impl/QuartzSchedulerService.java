package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.TaskRepository;
import com.sungardas.snapdirector.exception.SnapdirectorException;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.Job;
import com.sungardas.snapdirector.service.SchedulerService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.joda.time.DateTime;
import org.quartz.JobKey;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.impl.triggers.CronTriggerImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.quartz.MethodInvokingJobDetailFactoryBean;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import java.util.List;

@Service
public class QuartzSchedulerService implements SchedulerService {

    private static final Logger LOG = LogManager.getLogger(QuartzSchedulerService.class);

    @Autowired
    private Scheduler scheduler;

    @Autowired
    private ConfigurationService configurationService;

    @Autowired
    private TaskRepository taskRepository;

    @PostConstruct
    private void init() throws SchedulerException {
        scheduler.start();
        List<TaskEntry> tasks = taskRepository.findByInstanceIdAndRegular(configurationService.getConfiguration().getConfigurationId(), Boolean.TRUE.toString());
        for (TaskEntry taskEntry : tasks) {
            try {
                addTask(taskEntry);
            } catch (SnapdirectorException e) {
                LOG.error(e);
            }
        }
    }

    @PreDestroy
    private void destroy() throws SchedulerException {
        scheduler.shutdown();
    }

    @Override
    public void addTask(TaskEntry taskEntry) {
        if (TaskEntry.TaskEntryType.BACKUP.getType().equals(taskEntry.getType()) && taskEntry.getCron() != null && !taskEntry.getCron().isEmpty()) {
            if(Boolean.valueOf(taskEntry.getEnabled())) {
                addTask(new JobImpl(taskEntry), taskEntry.getCron());
            }
        } else {
            throw new SnapdirectorException("Invalid task: " + taskEntry);
        }
    }

    @Override
    public void addTask(Job job, String cronExpression) {
        try {
            MethodInvokingJobDetailFactoryBean jobDetail = new MethodInvokingJobDetailFactoryBean();
            jobDetail.setTargetObject(job);
            jobDetail.setTargetMethod("execute");
            jobDetail.setName(job.getId());
            jobDetail.setConcurrent(false);
            jobDetail.afterPropertiesSet();

            CronTriggerImpl cronTrigger = new CronTriggerImpl();
            cronTrigger.setName(job.getId() + "CRON");
            cronTrigger.setCronExpression(cronExpression);

            scheduler.scheduleJob(jobDetail.getObject(), cronTrigger);
        } catch (Exception e) {
            throw new SnapdirectorException(e);
        }
    }

    @Override
    public void removeTask(String id) {
        try {
            scheduler.deleteJob(new JobKey(id));
        } catch (SchedulerException e) {
            LOG.error(e);
        }
    }

    private class JobImpl implements Job {

        private TaskEntry taskEntry;

        public JobImpl(TaskEntry taskEntry) {
            this.taskEntry = taskEntry;
        }

        public void execute() {
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

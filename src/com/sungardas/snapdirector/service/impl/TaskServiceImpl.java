package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.TaskRepository;
import com.sungardas.snapdirector.dto.TaskDto;
import com.sungardas.snapdirector.dto.converter.TaskDtoConverter;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.exception.SnapdirectorException;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.SchedulerService;
import com.sungardas.snapdirector.service.TaskService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class TaskServiceImpl implements TaskService {

    private static final Logger LOG = LogManager.getLogger(TaskServiceImpl.class);

    @Autowired
    private TaskRepository taskRepository;
    @Value("${sungardas.worker.configuration}")
    private String configurationId;

    @Autowired
    private ConfigurationService configuration;

    @Autowired
    private SchedulerService schedulerService;

    @Override
    public void createTask(TaskDto taskDto) {
        TaskEntry newTask = TaskDtoConverter.convert(taskDto);
        String configurationId = configuration.getWorkerConfiguration().getConfigurationId();
        newTask.setWorker(configurationId);
        newTask.setInstanceId(configurationId);
        newTask.setStatus(TaskEntry.TaskEntryStatus.WAITING.getStatus());
        taskRepository.save(newTask);
        if (Boolean.valueOf(newTask.getRegular())) {
            try {
                schedulerService.addTask(newTask);
            } catch (SnapdirectorException e) {
                taskRepository.delete(newTask);
                LOG.error(e);
                throw e;
            }
        }
    }

    @Override
    public List<TaskDto> getAllTasks() {
        try {
            return TaskDtoConverter.convert(taskRepository.findByRegularAndInstanceId(Boolean.FALSE.toString(),
                    configuration.getWorkerConfiguration().getConfigurationId()));
        } catch (RuntimeException e) {
            LOG.error("Failed to get tasks.", e);
            throw new DataAccessException("Failed to get tasks.", e);
        }
    }

    @Override
    public List<TaskDto> getAllRegularTasks(String volumeId) {
        try {
            return TaskDtoConverter.convert(taskRepository.findByRegularAndVolumeAndInstanceId(Boolean.TRUE.toString(),
                    volumeId, configuration.getWorkerConfiguration().getConfigurationId()));
        } catch (RuntimeException e) {
            LOG.error("Failed to get tasks.", e);
            throw new DataAccessException("Failed to get tasks.", e);
        }
    }

    @Override
    public void removeTask(String id) {
        if (taskRepository.exists(id)) {
            TaskEntry taskEntry = taskRepository.findOne(id);
            if (TaskEntry.TaskEntryStatus.RUNNING.getStatus().equals(taskEntry.getStatus())) {
                throw new SnapdirectorException("Can`t remove task " + id + ", task in status: " + taskEntry.getStatus());
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

    @Override
    public void deleteAllTasks() {
        List<TaskEntry> taskList = taskRepository.findByInstanceId(configuration.getWorkerConfiguration().getConfigurationId());
        taskRepository.delete(taskList);
    }

    @Override
    public boolean isTableEmpty() {
        return taskRepository.count()==0;
    }


}

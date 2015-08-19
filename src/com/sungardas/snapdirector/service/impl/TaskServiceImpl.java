package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.TaskRepository;
import com.sungardas.snapdirector.dto.TaskDto;
import com.sungardas.snapdirector.dto.converter.TaskDtoConverter;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.service.ConfigurationService;
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


	@Override
	public void createTask(TaskDto taskDto) {
		TaskEntry newTask = TaskDtoConverter.convert(taskDto);
		String configurationId = configuration.getConfiguration().getConfigurationId();
		newTask.setWorker(configurationId);
		newTask.setInstanceId(configurationId);
		try {
			taskRepository.save(newTask);
		} catch (RuntimeException e) {
			LOG.error("Failed to save restore task.", e);
			throw new DataAccessException("Failed to save restore task.", e);
		}
	}

	@Override
	public List<TaskDto> getAllTasks() {
		try {
			return TaskDtoConverter.convert(taskRepository.findAll());
		} catch (RuntimeException e) {
			LOG.error("Failed to get tasks.", e);
			throw new DataAccessException("Failed to get tasks.", e);
		}
	}
}

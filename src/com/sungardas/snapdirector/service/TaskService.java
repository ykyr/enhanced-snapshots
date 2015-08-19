package com.sungardas.snapdirector.service;

import com.sungardas.snapdirector.dto.TaskDto;

import java.util.List;

public interface TaskService {
	void createTask(TaskDto taskDto);

	List<TaskDto> getAllTasks();
}

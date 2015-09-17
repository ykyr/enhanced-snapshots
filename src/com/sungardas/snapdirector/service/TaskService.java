package com.sungardas.snapdirector.service;

import com.sungardas.snapdirector.dto.TaskDto;

import java.util.List;

public interface TaskService {
    void createTask(TaskDto taskDto);

    List<TaskDto> getAllTasks();

    List<TaskDto> getAllRegularTasks(String volumeId);

    void removeTask(String Id);

    boolean isCanceled(String id);

    void updateTask(TaskDto taskInfo);
}

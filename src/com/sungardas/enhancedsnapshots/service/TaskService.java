package com.sungardas.enhancedsnapshots.service;

import com.sungardas.enhancedsnapshots.dto.TaskDto;

import java.util.List;

public interface TaskService {
    List<String> createTask(TaskDto taskDto);

    List<TaskDto> getAllTasks();

    List<TaskDto> getAllRegularTasks(String volumeId);

    void removeTask(String Id);

    boolean isCanceled(String id);

    void updateTask(TaskDto taskInfo);

    List<TaskDto> getAllTasks(String volumeId);
}

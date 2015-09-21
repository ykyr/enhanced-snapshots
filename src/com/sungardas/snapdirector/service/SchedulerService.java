package com.sungardas.snapdirector.service;

import java.util.Set;

import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;

public interface SchedulerService {
    void addTask(TaskEntry taskEntry);

    void addTask(Task task, String cronExpression);

    void removeTask(String id);

    Set<String> getVolumeIdsWithSchedule();
}

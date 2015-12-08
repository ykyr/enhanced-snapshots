package com.sungardas.enhancedsnapshots.service;

import com.sungardas.enhancedsnapshots.dto.Dto;
import com.sungardas.enhancedsnapshots.dto.ExceptionDto;
import com.sungardas.enhancedsnapshots.dto.TaskProgressDto;

public interface NotificationService {

    void notifyAboutTaskProgress(String taskId, String message, double progress);

    void notifyAboutTaskProgress(TaskProgressDto dto);

    void notifyAboutError(ExceptionDto exceptionDto);

    void notifyUser(String broker, Dto dto);
}

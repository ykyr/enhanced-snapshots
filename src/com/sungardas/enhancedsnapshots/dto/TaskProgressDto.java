package com.sungardas.enhancedsnapshots.dto;

public class TaskProgressDto implements Dto {
    private String taskId;

    private String message;

    private double progress;

    public TaskProgressDto() {
    }

    public TaskProgressDto(String taskId, String message, double progress) {
        this.taskId = taskId;
        this.message = message;
        this.progress = progress;
    }

    public String getTaskId() {
        return taskId;
    }

    public void setTaskId(String taskId) {
        this.taskId = taskId;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public double getProgress() {
        return progress;
    }

    public void setProgress(double progress) {
        this.progress = progress;
    }
}

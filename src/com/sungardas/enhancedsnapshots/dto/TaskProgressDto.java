package com.sungardas.enhancedsnapshots.dto;

public class TaskProgressDto implements Dto {
    private String taskId;

    private String type;

    private String state;

    private long size;

    private long currentProgress;

    public String getTaskId() {
        return taskId;
    }

    public void setTaskId(String taskId) {
        this.taskId = taskId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public long getSize() {
        return size;
    }

    public void setSize(long size) {
        this.size = size;
    }

    public long getCurrentProgress() {
        return currentProgress;
    }

    public void setCurrentProgress(long currentProgress) {
        this.currentProgress = currentProgress;
    }

    public enum TaskExecutionState {
        STARTING,
        COPYING,
        DETACH_VOLUME,
        DELETING_TEMP_VOLUME,
        CREATE_VOLUME_FROM_SNAPSHOT,
        CREATE_VOLUME,
        ATTACH_VOLUME,
        DONE
    }
}

package com.sungardas.enhancedsnapshots.dto;

public class CopyingTaskProgressDto extends TaskProgressDto {
    public static final long BYTES_IN_MEGABYTE = 1000000;

    private double progressMin;
    private double progressMax;

    public CopyingTaskProgressDto(final String taskId, final double progressMin, final double progressMax) {
        setTaskId(taskId);
        this.progressMax = progressMax;
        this.progressMin = progressMin;
    }

    public void setCopyingProgress(final long progress, final long volumeSize){
        double p = ((double)progress)/volumeSize;
        setProgress(progressMin + (((progressMax - progressMin) / 100) * (p * 100)));
        setMessage("Copying: " + (progress / BYTES_IN_MEGABYTE) + "/" + (volumeSize / BYTES_IN_MEGABYTE) + "MB");
    }
}

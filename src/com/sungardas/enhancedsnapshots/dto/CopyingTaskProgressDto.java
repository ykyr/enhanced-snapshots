package com.sungardas.enhancedsnapshots.dto;

public class CopyingTaskProgressDto extends TaskProgressDto {
    public static final long BYTES_IN_MEGABYTE = 1000000;
    public static final double MEGABYTE_IN_GIBIBYTE = 1073.74;

    private double progressMin;
    private double progressMax;
    private long volumeSize;

    public CopyingTaskProgressDto(final String taskId, final double progressMin, final double progressMax, long volumeSize) {
        setTaskId(taskId);
        this.progressMax = progressMax;
        this.progressMin = progressMin;
        this.volumeSize = (long) (volumeSize * MEGABYTE_IN_GIBIBYTE) * BYTES_IN_MEGABYTE;
    }

    public void setCopyingProgress(final long progress) {
        double p = ((double)progress)/volumeSize;
        setProgress(progressMin + (((progressMax - progressMin) / 100) * (p * 100)));
        setMessage("Copying: " + (progress / BYTES_IN_MEGABYTE) + "/" + (volumeSize / BYTES_IN_MEGABYTE) + "MB");
    }
}

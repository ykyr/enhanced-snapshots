package com.sungardas.enhancedsnapshots.service;

import com.sun.management.OperatingSystemMXBean;

import java.io.File;
import java.lang.management.ManagementFactory;

public interface SDFSStateService {

    long BYTES_IN_GB = 1_073_741_824;
    int SDFS_VOLUME_SIZE_IN_GB_PER_GB_OF_RAM = 2000;
    long SYSTEM_RESERVED_RAM_IN_BYTES = BYTES_IN_GB / 4;

    // Reserved storage for logs of Tomcat and other services
    long SYSTEM_RESERVED_STORAGE_IN_BYTES = BYTES_IN_GB / 2;
    long SDFS_RESERVED_RAM_IN_BYTES = BYTES_IN_GB;

    /**
     * Returns max sdfs volume size for current system in GB
     * @return
     */
    static int getMaxVolumeSize(boolean sdfsRunning) {
        OperatingSystemMXBean osBean = (OperatingSystemMXBean) ManagementFactory.getOperatingSystemMXBean();
        //Total RAM - RAM available for Tomcat - reserved
        long totalRAM = osBean.getFreePhysicalMemorySize() - Runtime.getRuntime().freeMemory() - SYSTEM_RESERVED_RAM_IN_BYTES;
        if (!sdfsRunning) {
            totalRAM = totalRAM - SDFS_RESERVED_RAM_IN_BYTES;
        }
        int maxVolumeSize = (int) (totalRAM / BYTES_IN_GB) * SDFS_VOLUME_SIZE_IN_GB_PER_GB_OF_RAM;
        return maxVolumeSize;
    }


    /**
     * Returns count of GB which can be used to increase sdfs local cache
     * @return
     */
    static int getFreeStorageSpace() {
        File file = new File("/");
        int maxLocalCacheInGb = (int) ((file.getFreeSpace() - SYSTEM_RESERVED_STORAGE_IN_BYTES) / BYTES_IN_GB);
        return maxLocalCacheInGb;

    }

    /**
     * Backup SDFS metadata to S3
     */
    void backupState(String taskId);

    /**
     * Return true when S3 bucket contains SDFS backup, false otherwise
     */
    boolean containsSdfsMetadata(String sBucket);

    Long getBackupTime();

    /**
     * Reconfigure SDFS and restart
     */
    void reconfigureAndRestartSDFS();

    /**
     * Restore SDFS from S3 bucket
     */
    void restoreSDFS();

    /**
     * Start SDFS if it is not running
     */
    void startSDFS();

    /**
     * Stop SDFS if it is not running
     */
    void stopSDFS();

    /**
     * Return true if SDFS is currently runnings, false otherwise
     */
    boolean sdfsIsAvailable();

    /**
     * Expand sdfs volume
     */
    void expandSdfsVolume(String newVolumeSize);


}

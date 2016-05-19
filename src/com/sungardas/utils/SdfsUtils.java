package com.sungardas.utils;


import com.sun.management.UnixOperatingSystemMXBean;

import java.lang.management.ManagementFactory;

public class SdfsUtils {

    private static final long BYTES_IN_GB = 1_073_741_824;
    private static final int SDFS_VOLUME_SIZE_IN_GB_PER_GB_OF_RAM = 2000;
    private static final long SYSTEM_RESERVED_RAM_IN_BYTES = BYTES_IN_GB/4;
    private static final long SDFS_RESERVED_RAM_IN_BYTES = BYTES_IN_GB;

    /**
     * Returns max sdfs volume size for current system in GB
     * @return
     */
    public static int getMaxVolumeSize() {
        UnixOperatingSystemMXBean osBean = (UnixOperatingSystemMXBean) ManagementFactory.getOperatingSystemMXBean();
        //Total RAM - RAM available for Tomcat - reserved
        long totalRAM = osBean.getFreePhysicalMemorySize() - Runtime.getRuntime().freeMemory() - SYSTEM_RESERVED_RAM_IN_BYTES - SDFS_RESERVED_RAM_IN_BYTES;
        int maxVolumeSize = (int) (totalRAM / BYTES_IN_GB) * SDFS_VOLUME_SIZE_IN_GB_PER_GB_OF_RAM;
        return maxVolumeSize;
    }

    /**
     * Returns max sdfs local cache size for current system in GB
     * @return
     */
    public static int getMaxLocalCacheSize(){
        return 0;

    }

}

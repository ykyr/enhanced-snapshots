package com.sungardas.enhancedsnapshots.service;

import com.sungardas.enhancedsnapshots.dto.SystemConfiguration;

/**
 * Enhancedsnapshots system service interface main responsibilities: System backup & restore and configuration
 */
public interface SystemService {

    /**
     * Backup current system state to S3 bucket Backup data are: -DynamoDB tables -Property file -nginx certificates
     */
    void backup();

    /**
     * Restore system state from backup {@link #backup()}
     */
    void restore();

    SystemConfiguration getSystemConfiguration();

    void setSystemConfiguration(SystemConfiguration systemConfiguration);

    String VOLUME_SIZE_UNIT = "GB";
}

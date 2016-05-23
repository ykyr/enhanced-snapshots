package com.sungardas.enhancedsnapshots.service;

/**
 * Enhancedsnapshots system service interface main responsibilities: System backup & restore TODO move all logic from
 * {@link ConfigurationService}
 */
public interface SystemService {

    /**
     * Backup current system state to S3 bucket Backup data are: -DynamoDB tables -Property file -nginx certificates
     */
    void backup();

    /**
     * Restore system state from backup {@link #backup()}
     *
     * @param bucketName Amazon S3 bucket name with backup
     */
    void restore(String bucketName);
}

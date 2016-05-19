package com.sungardas.enhancedsnapshots.service;

import com.sungardas.enhancedsnapshots.dto.SystemConfiguration;

public interface ConfigurationService {

	SystemConfiguration getSystemConfiguration();

	void setSystemProperties (SystemConfiguration.SystemProperties systemProperties);

	String getRegion();

	String getS3Bucket();

	String getConfigurationId();

	int getAmazonRetryCount();

	int getAmazonRetrySleep();

	int getMaxQueueSize();

	String getRetentionCronExpression();

	int getWorkerDispatcherPollingRate();

	String getTempVolumeType();

	int getTempVolumeIopsPerGb();

	String getRestoreVolumeType();

	int getRestoreVolumeIopsPerGb();

	String getSdfsVolumeName();

	String getSdfsMountPoint();

	String getSdfsLocalCacheSize();

	String getSdfsVolumeSize();

	String getSdfsConfigPath();

	String getSdfsBackupFileName();

	public int getWaitTimeBeforeNewSyncWithAWS();

	public int getMaxWaitTimeToDetachVolume();

}

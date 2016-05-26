package com.sungardas.enhancedsnapshots.service;

import com.sungardas.enhancedsnapshots.dto.SystemConfiguration;

public interface ConfigurationService {

	SystemConfiguration getSystemConfiguration();

	void setSystemConfiguration (SystemConfiguration systemConfiguration);

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

	int getSdfsVolumeSizeWithoutMeasureUnit();

	int getSdfsLocalCacheSizeWithoutMeasureUnit();

	String getSdfsConfigPath();

	String getSdfsBackupFileName();

	int getWaitTimeBeforeNewSyncWithAWS();

	int getMaxWaitTimeToDetachVolume();

	String getVolumeSizeUnit();
}

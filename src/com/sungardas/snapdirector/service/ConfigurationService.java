package com.sungardas.snapdirector.service;

import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.dto.SystemConfiguration;

public interface ConfigurationService {

	boolean isTableEmpty();

	void reload();

	WorkerConfiguration getWorkerConfiguration();

	SystemConfiguration getSystemConfiguration();

	void deleteConfiguration();
}

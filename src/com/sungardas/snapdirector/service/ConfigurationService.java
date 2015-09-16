package com.sungardas.snapdirector.service;

import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.dto.SystemConfiguration;

public interface ConfigurationService {

	void reload();

	WorkerConfiguration getWorkerConfiguration();

	SystemConfiguration getSystemConfiguration();

}

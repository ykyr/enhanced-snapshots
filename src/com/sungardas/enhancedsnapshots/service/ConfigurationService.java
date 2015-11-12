package com.sungardas.enhancedsnapshots.service;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.enhancedsnapshots.dto.SystemConfiguration;

public interface ConfigurationService {

	void reload();

	WorkerConfiguration getWorkerConfiguration();

	SystemConfiguration getSystemConfiguration();

	boolean isNewVersionAvailable();

}

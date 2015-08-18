package com.sungardas.snapdirector.service;

import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;

public interface ConfigurationService {

	void reload();

	WorkerConfiguration getConfiguration();

}

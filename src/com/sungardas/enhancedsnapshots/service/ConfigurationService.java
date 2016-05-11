package com.sungardas.enhancedsnapshots.service;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.Configuration;
import com.sungardas.enhancedsnapshots.dto.SystemConfiguration;

public interface ConfigurationService {

	void reload();

	Configuration getConfiguration();

	SystemConfiguration getSystemConfiguration();

	void setSystemProperties (SystemConfiguration.SystemProperties systemProperties);

}

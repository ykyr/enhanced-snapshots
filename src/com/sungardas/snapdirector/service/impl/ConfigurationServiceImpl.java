package com.sungardas.snapdirector.service.impl;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Scanner;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.aws.dynamodb.repository.WorkerConfigurationRepository;
import com.sungardas.snapdirector.service.ConfigurationService;

@Service
public class ConfigurationServiceImpl implements ConfigurationService{
	private  final Logger LOG = LogManager.getLogger(ConfigurationServiceImpl.class);

	@Value("${sungardas.worker.configuration}")
	private String fakeConfigurationId;
	
	@Autowired
	WorkerConfigurationRepository configurationRepository;
	
	WorkerConfiguration currectConfiguration;
	
	@Override
	public WorkerConfiguration getConfiguration() {
		if(currectConfiguration == null){
			currectConfiguration = configurationRepository.findOne(getConfigurationId());
		}
		return currectConfiguration;
	}
	
	@Override
	public void reload() {
		currectConfiguration = null;
	}
		
	

	private String getConfigurationId() {
		String instanceId = null;
		try {
			URL url = new URL("http://169.254.169.254/latest/meta-data/instance-id");
			URLConnection conn = url.openConnection();
			Scanner s = new Scanner(conn.getInputStream());
			if (s.hasNext()) {
				instanceId = s.next();
				LOG.info("Getting Worker InstanceId from metadata: " + instanceId);
			}
			s.close();
		} catch (IOException e) {
			instanceId = fakeConfigurationId;
		}
		return instanceId;
	
	
	}
}

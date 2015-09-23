package com.sungardas.enhancedsnapshots.dto.converter;

import java.util.ArrayList;
import java.util.List;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.enhancedsnapshots.dto.WorkerConfigurationDto;

public final class WorkerConfigurationConverter {
	private WorkerConfigurationConverter() {
	}
	
	public static WorkerConfigurationDto convert(WorkerConfiguration workerConfiguration) {
		WorkerConfigurationDto dto = new WorkerConfigurationDto();
		dto.setWorkerId(workerConfiguration.getConfigurationId());
		dto.setUseFakeEC2(workerConfiguration.isUseFakeEC2());
		dto.setUseFakeBackup(workerConfiguration.isUseFakeBackup());
		dto.setTaskQueueURL(workerConfiguration.getTaskQueueURL());
		dto.setSdfsVolumeName(workerConfiguration.getSdfsVolumeName());
		dto.setSdfsMountPoint(workerConfiguration.getSdfsMountPoint());
		dto.setSdfsBucket(null);
		return dto;
	}
	
	public static List<WorkerConfigurationDto> convert(Iterable<WorkerConfiguration> configurations) {
		List<WorkerConfigurationDto> dtos = new ArrayList<>();
        for(WorkerConfiguration configuration: configurations){
            dtos.add(convert(configuration));
        }
        return dtos;
	}

}

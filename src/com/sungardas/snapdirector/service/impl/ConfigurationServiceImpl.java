package com.sungardas.snapdirector.service.impl;


import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.aws.dynamodb.repository.WorkerConfigurationRepository;
import com.sungardas.snapdirector.dto.SystemConfiguration;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.SDFSStateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class ConfigurationServiceImpl implements ConfigurationService {

    @Value("${sungardas.worker.configuration}")
    private String instanceId;

    @Value("${amazon.s3.bucket}")
    private String s3BucketName;

    @Value("${snapdirector.sdfs.default.size}")
    private String defaultVolumeSize;

    @Autowired
    WorkerConfigurationRepository configurationRepository;

    WorkerConfiguration currectConfiguration;

    @Autowired
    SDFSStateService sdfsStateService;

    @Override
    public WorkerConfiguration getWorkerConfiguration() {
        if (currectConfiguration == null) {
            currectConfiguration = configurationRepository.findOne(instanceId);
        }
        return currectConfiguration;
    }

    @Override
    public SystemConfiguration getSystemConfiguration() {
        SystemConfiguration configuration = new SystemConfiguration();
        configuration.setQueue(new SystemConfiguration.Queue());
        configuration.getQueue().setQueueName(currectConfiguration.getTaskQueueURL());

        configuration.setS3(new SystemConfiguration.S3());
        configuration.getS3().setBucketName(s3BucketName);

        configuration.setSdfs(new SystemConfiguration.SDFS());
        configuration.getSdfs().setMountPoint(currectConfiguration.getSdfsMountPoint());
        configuration.getSdfs().setVolumeName(currectConfiguration.getSdfsVolumeName());
        configuration.getSdfs().setVolumeSize(defaultVolumeSize);

        configuration.setLastBackup(sdfsStateService.getBackupTime());
        return configuration;
    }

	@Override
	public void reload() {
		currectConfiguration = null;
	}
}

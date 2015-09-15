package com.sungardas.snapdirector.service.impl;


import com.amazonaws.services.sqs.AmazonSQS;
import com.sungardas.snapdirector.aws.dynamodb.model.*;
import com.sungardas.snapdirector.aws.dynamodb.repository.*;
import com.sungardas.snapdirector.service.RemoveAppConfiguration;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import javax.annotation.PostConstruct;
import java.util.List;

public class RemoveAppConfigurationImpl implements RemoveAppConfiguration {
    @Autowired
    private AmazonSQS sqs;

    @Autowired
    SnapshotRepository snapshotRepository;

    @Autowired
    BackupRepository backupRepository;

    @Autowired
    RetentionRepository retentionRepository;

    @Autowired
    TaskRepository taskRepository;

    @Autowired
    UserRepository userRepository;

    @Autowired WorkerConfigurationRepository configurationRepository;

    @Value("${sungardas.worker.configuration}")
    private String configurationId;

    WorkerConfiguration configuration;
    @PostConstruct
    private void init() {
        configuration = configurationRepository.findOne(configurationId);
    }

    @Override
    public String getConfigurationId() {
        return configurationId;
    }

    @Override
    public void dropConfiguration() {
    }

    private void dropS3Bucket() {

    }

    private void dropQueue() {
        String queueURL= configuration.getTaskQueueURL();
        sqs.deleteQueue(queueURL);
        sqs.shutdown();
    }

    private void dropDbData() {
        List<User> userList = userRepository.findByInstanceId(configurationId);
        userRepository.delete(userList);

        List<TaskEntry> taskList = taskRepository.findByInstanceId(configurationId);
        taskRepository.delete(taskList);

        List<RetentionEntry> retentionList = retentionRepository.findByInstanceId(configurationId);
        retentionRepository.delete(retentionList);

        List<BackupEntry> backupList = backupRepository.findAll(configurationId);


    }

}

package com.sungardas.snapdirector.service.impl;

import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.util.EC2MetadataUtils;
import com.sungardas.snapdirector.aws.dynamodb.model.*;
import com.sungardas.snapdirector.aws.dynamodb.repository.*;
import com.sungardas.snapdirector.exception.OperationNotAllowedException;
import com.sungardas.snapdirector.service.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;

@Service
public class RemoveAppConfigurationImpl implements RemoveAppConfiguration {
    @Autowired
    private AmazonSQS sqs;

    @Autowired
    private SnapshotService snapshotService;

    @Autowired
    private BackupService backupService;

    @Autowired
    private RetentionService retentionService;

    @Autowired
    private TaskService taskService;

    @Autowired
    private UserService userService;

    @Autowired
    private UserRepository userRepository;

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
    public void dropConfiguration(String currentUserEmail, String instanceId) {
        if (!userService.isAdmin(currentUserEmail)) {
            throw new OperationNotAllowedException("Only admin can delete service");
        }
        if (!instanceId.equals(EC2MetadataUtils.getInstanceId())) {
            throw new OperationNotAllowedException("Provided instance ID is incorrect");
        }

//        dropS3Bucket();
//        dropQueue();
//        dropDbData();
    }

    private void dropS3Bucket() {

    }

    private void stopSDFS(){

    }

    private void terminateInstance(){

    }

    private void dropQueue() {
        String queueURL= configuration.getTaskQueueURL();
        sqs.deleteQueue(queueURL);
        sqs.shutdown();
    }

    private void dropDbData() {
        userService.deleteAllUsers();
        taskService.deleteAllTasks();
        retentionService.deleteAllRetentions();
        backupService.deleteAllBackups();
        snapshotService.deleteAllSnapshots();

        //TODO: remove tables in case they are empty

    }
}

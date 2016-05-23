package com.sungardas.destroy;

import java.util.List;

import javax.annotation.PostConstruct;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.model.TerminateInstancesRequest;
import com.amazonaws.services.s3.AmazonS3;

import com.amazonaws.util.EC2MetadataUtils;

import com.sungardas.enhancedsnapshots.aws.AmazonConfigProvider;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.*;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.*;
import com.sungardas.enhancedsnapshots.service.AWSCommunicationService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;

public class RemoveAppConfiguration {

    @Value("${enhancedsnapshots.db.tables}")
    private String[] tables;

    @Autowired
    @Qualifier("amazonDynamoDB")
    private AmazonDynamoDB db;

    @Autowired
    private AmazonS3 s3;

    @Autowired
    private AmazonEC2 ec2;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private TaskRepository taskRepository;

    @Autowired
    private RetentionRepository retentionRepository;

    @Autowired
    private BackupRepository backupRepository;

    @Autowired
    private SnapshotRepository snapshotRepository;

    @Autowired
    private ConfigurationRepository configurationRepository;

    @Autowired
    private AWSCommunicationService awsCommunicationService;

    private DynamoDB dynamoDB;

    private String configurationId;

    @PostConstruct
    private void init() {
        configurationId = EC2MetadataUtils.getInstanceId();
        dynamoDB = new DynamoDB(db);
        dropConfiguration();
    }

    private void dropConfiguration() {
        awsCommunicationService.dropS3Bucket(getConfiguration().getS3Bucket());
        dropDbData();
        terminateInstance();
    }



    private void terminateInstance() {
        ec2.terminateInstances(new TerminateInstancesRequest().withInstanceIds(configurationId));
    }

    private void dropDbData() {
        deleteAllUsers();
        deleteAllTasks();
        deleteAllRetentions();
        deleteAllBackups();
        deleteAllSnapshots();
        deleteConfiguration();

        if (isTablesEmpty()) {
            for (String tableToDrop : tables) {
                dropTable(tableToDrop);
            }
        }

    }

    private boolean isTablesEmpty() {
        long count = userRepository.count()
                + taskRepository.count()
                + retentionRepository.count()
                + backupRepository.count()
                + snapshotRepository.count()
                + configurationRepository.count();
        return count == 0;
    }

    private void deleteConfiguration() {
        configurationRepository.delete(configurationId);
    }

    private void deleteAllSnapshots() {
        List<SnapshotEntry> snapshotList = snapshotRepository.findByInstanceId(configurationId);
        snapshotRepository.delete(snapshotList);
    }

    private void deleteAllBackups() {
        List<BackupEntry> backupList = backupRepository.findAll(configurationId);
        for (BackupEntry entry : backupList) {
            backupRepository.delete(entry);
        }
    }

    private void deleteAllRetentions() {
        List<RetentionEntry> retentionList = retentionRepository.findByInstanceId(configurationId);
        retentionRepository.delete(retentionList);
    }

    private void deleteAllTasks() {
        List<TaskEntry> taskList = taskRepository.findByInstanceId(configurationId);
        taskRepository.delete(taskList);
    }

    private void deleteAllUsers() {
        List<User> userList = userRepository.findByInstanceId(configurationId);
        userRepository.delete(userList);
    }

    private void dropTable(String tableName) {
        Table tableToDelete = dynamoDB.getTable(AmazonConfigProvider.getDynamoDbPrefix() + tableName);
        tableToDelete.delete();
        try {
            tableToDelete.waitForDelete();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private Configuration getConfiguration(){
        return configurationRepository.findOne(configurationId);
    }
}

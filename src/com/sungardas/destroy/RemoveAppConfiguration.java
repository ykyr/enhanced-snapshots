package com.sungardas.destroy;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.model.TerminateInstancesRequest;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.*;
import com.amazonaws.services.sqs.AmazonSQS;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.*;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import javax.annotation.PostConstruct;
import java.util.Iterator;
import java.util.List;

public class RemoveAppConfiguration {

    @Value("${enhancedsnapshots.db.tables}")
    private String[] tables;

    @Autowired
    private AmazonSQS sqs;

    @Autowired
    private AmazonDynamoDB db;

    @Autowired
    private AmazonS3 s3;

    @Autowired
    private AmazonEC2 ec2;

    @Value("${sungardas.worker.configuration}")
    private String configurationId;

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
    private WorkerConfigurationRepository configurationRepository;

    private DynamoDB dynamoDB;

    private WorkerConfiguration configuration;

    @PostConstruct
    private void init() {
        configuration = configurationRepository.findOne(configurationId);
        dynamoDB = new DynamoDB(db);
        dropConfiguration();
    }

    private void dropConfiguration() {
        dropS3Bucket();
        dropQueue();
        dropDbData();

        terminateInstance();
    }

    private void dropS3Bucket() {
        String bucketName = configuration.getS3Bucket();
        ObjectListing objectListing = s3.listObjects(bucketName);

        while (true) {
            for (Iterator<?> iterator = objectListing.getObjectSummaries().iterator(); iterator.hasNext(); ) {
                S3ObjectSummary objectSummary = (S3ObjectSummary) iterator.next();
                s3.deleteObject(bucketName, objectSummary.getKey());
            }

            if (objectListing.isTruncated()) {
                objectListing = s3.listNextBatchOfObjects(objectListing);
            } else {
                break;
            }
        }
        VersionListing list = s3.listVersions(new ListVersionsRequest().withBucketName(bucketName));
        for (Iterator<?> iterator = list.getVersionSummaries().iterator(); iterator.hasNext(); ) {
            S3VersionSummary s = (S3VersionSummary) iterator.next();
            s3.deleteVersion(bucketName, s.getKey(), s.getVersionId());
        }

        s3.deleteBucket(configuration.getS3Bucket());
    }

    private void terminateInstance() {
        ec2.terminateInstances(new TerminateInstancesRequest().withInstanceIds(configurationId));
    }

    private void dropQueue() {
        String queueURL = configuration.getTaskQueueURL();
        sqs.deleteQueue(queueURL);
        sqs.shutdown();
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
        return count==0;
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
        Table tableToDelete = dynamoDB.getTable(tableName);
        tableToDelete.delete();
        try {
            tableToDelete.waitForDelete();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}

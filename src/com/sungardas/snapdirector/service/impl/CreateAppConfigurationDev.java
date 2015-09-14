package com.sungardas.snapdirector.service.impl;

import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.model.CreateQueueRequest;
import com.sungardas.snapdirector.aws.dynamodb.model.User;
import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.beans.factory.annotation.Autowired;

import javax.annotation.PostConstruct;

public class CreateAppConfigurationDev {

    @Autowired
    private AmazonDynamoDB amazonDynamoDB;

    @Autowired
    private AmazonSQS amazonSQS;

    @PostConstruct
    private void init() {
        DynamoDBMapper mapper = new DynamoDBMapper(amazonDynamoDB);
        WorkerConfiguration workerConfiguration = getDevConf();
        mapper.save(workerConfiguration);

        User user = new User("dev", "dev", "admin@admin", DigestUtils.sha512Hex("admin"), "admin");
        mapper.save(user);

        try {
            CreateQueueRequest createQueueRequest = new CreateQueueRequest()
                    .withQueueName("DEV");
            amazonSQS.createQueue(createQueueRequest);
        } catch (Exception e) {

        }
    }

    private WorkerConfiguration getDevConf() {
        WorkerConfiguration workerConfiguration = new WorkerConfiguration();
        workerConfiguration.setConfigurationId("DEV");
        workerConfiguration.setEc2Region(Regions.EU_WEST_1.getName());
        workerConfiguration.setFakeBackupSource(null);
        workerConfiguration.setSdfsMountPoint("");
        workerConfiguration.setSdfsVolumeName("");
        workerConfiguration.setTaskQueueURL("DEV");
        workerConfiguration.setUseFakeBackup(true);
        workerConfiguration.setUseFakeEC2(true);
        return workerConfiguration;
    }
}
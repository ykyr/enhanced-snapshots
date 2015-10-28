package com.sungardas.enhancedsnapshots.service.impl;

import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.WorkerConfiguration;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.beans.factory.annotation.Autowired;

import javax.annotation.PostConstruct;

public class CreateAppConfigurationDev {

    @Autowired
    private AmazonDynamoDB amazonDynamoDB;

    @PostConstruct
    private void init() {
        DynamoDBMapper mapper = new DynamoDBMapper(amazonDynamoDB);
        WorkerConfiguration workerConfiguration = getDevConf();
        mapper.save(workerConfiguration);

        User user = new User("admin@admin", DigestUtils.sha512Hex("admin"), "admin", "dev", "dev", "DEV");
        user.setId("DEV");
        mapper.save(user);
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


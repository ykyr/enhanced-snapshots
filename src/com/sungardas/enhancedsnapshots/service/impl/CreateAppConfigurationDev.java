package com.sungardas.enhancedsnapshots.service.impl;

import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.Configuration;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import javax.annotation.PostConstruct;

public class CreateAppConfigurationDev {

    @Value("${enhancedsnapshots.default.tempVolumeType}")
    private String tempVolumeType;
    @Value("${enhancedsnapshots.default.tempVolumeIopsPerGb}")
    private int tempVolumeIopsPerGb;
    @Value("${enhancedsnapshots.default.restoreVolumeType}")
    private String restoreVolumeType;
    @Value("${enhancedsnapshots.default.restoreVolumeIopsPerGb}")
    private int restoreVolumeIopsPerGb;

    @Autowired
    private AmazonDynamoDB amazonDynamoDB;

    @PostConstruct
    private void init() {
        DynamoDBMapper mapper = new DynamoDBMapper(amazonDynamoDB);
        Configuration configuration = getDevConf();
        mapper.save(configuration);

        User user = new User("admin@admin", DigestUtils.sha512Hex("admin"), "admin", "dev", "dev", "DEV");
        user.setId("DEV");
        mapper.save(user);
    }

    private Configuration getDevConf() {
        Configuration configuration = new Configuration();
        configuration.setConfigurationId("DEV");
        configuration.setEc2Region(Regions.EU_WEST_1.getName());
        configuration.setSdfsMountPoint("");
        configuration.setSdfsVolumeName("");
        configuration.setRestoreVolumeIopsPerGb(restoreVolumeIopsPerGb);
        configuration.setRestoreVolumeType(restoreVolumeType);
        configuration.setTempVolumeIopsPerGb(tempVolumeIopsPerGb);
        configuration.setTempVolumeType(tempVolumeType);
        return configuration;
    }
}


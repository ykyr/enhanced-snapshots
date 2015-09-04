package com.sungardas.snapdirector.service.impl;


import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.model.*;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.model.CreateQueueRequest;
import com.sungardas.snapdirector.aws.dynamodb.model.User;
import com.sungardas.snapdirector.aws.dynamodb.repository.UserRepository;
import com.sungardas.snapdirector.dto.InitConfigurationDto;
import com.sungardas.snapdirector.dto.UserDto;
import com.sungardas.snapdirector.dto.converter.UserDtoConverter;
import com.sungardas.snapdirector.exception.ConfigurationException;
import com.sungardas.snapdirector.service.CreateAppConfiguration;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.io.IOException;
import java.util.ArrayList;
import java.util.concurrent.TimeUnit;

@Profile("prod")
@Service
public class CreateAppConfigurationImpl implements CreateAppConfiguration {
    private static final Log LOG = LogFactory.getLog(CreateAppConfigurationImpl.class);

    @Value("${amazon.aws.accesskey:}")
    private String amazonAWSAccessKey;
    @Value("${amazon.aws.secretkey}")
    private String amazonAWSSecretKey;

    @Autowired private SharedDataServiceImpl sharedDataService;

    @Autowired private AmazonDynamoDB amazonDynamoDB;
    @Autowired private AmazonSQS amazonSQS;

    @Autowired private UserRepository userRepository;

    @PostConstruct
    private void createConfiguration() {
        InitConfigurationDto initConfigurationDto =  sharedDataService.getInitConfigurationDto();
        boolean createDB = !initConfigurationDto.getDb().isValid();
       if(createDB) createDB();
        createTaskQueue();
        if(initConfigurationDto.getSdfs().isCreated()) createSDFS();

        createSDFS();


    }

    private void createDB() {
        createDbStructure();
        createAdminUserIfProvided();
    }

    private void createDbStructure() throws ConfigurationException {
        createTable("BackupList", 1L, 1L, "volumeId", "S", "fileName ", "S");
        createTable("Configurations", 1L, 1L, "configurationId ", "S");
        createTable("Retention", 1L, 1L, "volumeId  ", "S");
        createTable("Schedule", 1L, 1L, "id  ", "S");
        createTable("Tasks", 1L, 1L, "id  ", "S");
        createTable("Users", 1L, 1L, "email   ", "S");
        createTable("Snapshots", 1L, 1L, "id   ", "S");
    }

    private void createTable(
            String tableName, long readCapacityUnits, long writeCapacityUnits,
            String hashKeyName, String hashKeyType) {

        createTable(tableName, readCapacityUnits, writeCapacityUnits,
                hashKeyName, hashKeyType, null, null);
    }

    private void createTable(
            String tableName, long readCapacityUnits, long writeCapacityUnits,
            String hashKeyName, String hashKeyType,
            String rangeKeyName, String rangeKeyType) {

        DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);

        try {

            ArrayList<KeySchemaElement> keySchema = new ArrayList<>();
            keySchema.add(new KeySchemaElement()
                    .withAttributeName(hashKeyName)
                    .withKeyType(KeyType.HASH));

            ArrayList<AttributeDefinition> attributeDefinitions = new ArrayList<AttributeDefinition>();
            attributeDefinitions.add(new AttributeDefinition()
                    .withAttributeName(hashKeyName)
                    .withAttributeType(hashKeyType));

            if (rangeKeyName != null) {
                keySchema.add(new KeySchemaElement()
                        .withAttributeName(rangeKeyName)
                        .withKeyType(KeyType.RANGE));
                attributeDefinitions.add(new AttributeDefinition()
                        .withAttributeName(rangeKeyName)
                        .withAttributeType(rangeKeyType));
            }

            CreateTableRequest request = new CreateTableRequest()
                    .withTableName(tableName)
                    .withKeySchema(keySchema)
                    .withProvisionedThroughput( new ProvisionedThroughput()
                            .withReadCapacityUnits(readCapacityUnits)
                            .withWriteCapacityUnits(writeCapacityUnits));


            request.setAttributeDefinitions(attributeDefinitions);

            LOG.info("Issuing CreateTable request for " + tableName);
            CreateTableResult createResult = amazonDynamoDB.createTable(request);

            Table table = dynamoDB.createTable(request);
            LOG.info("Waiting for " + tableName
                    + " to be created...this may take a while...");
            table.waitForActive();

        } catch (Exception e) {
            LOG.error("CreateTable request failed for " + tableName, e);
            throw new ConfigurationException("CreateTable request failed for " + tableName,e);
        }
    }

    private void createAdminUserIfProvided() {
        UserDto userDto = sharedDataService.getAdminUser();
        String password = sharedDataService.getAdminPassword();
        if(userDto!=null && password != null) {
            User userToCreate = UserDtoConverter.convert(userDto);
            userToCreate.setPassword(password);
            userRepository.save(userToCreate);
        }
    }

    private void createTaskQueue() {
        boolean deleteFirst = sharedDataService.getInitConfigurationDto().getQueue().isCreated();
        String queue =  sharedDataService.getInitConfigurationDto().getQueue().getQueueName();
        if(deleteFirst) {
            amazonSQS.deleteQueue(queue);
            try {
                TimeUnit.SECONDS.sleep(65);
            } catch (InterruptedException e) {}
        }

        CreateQueueRequest createQueueRequest = new CreateQueueRequest()
                .withQueueName(queue);
        amazonSQS.createQueue(createQueueRequest);

    }

    private void createSDFS() {
        boolean bucketAlreadyExists = sharedDataService.getInitConfigurationDto().getS3().isCreated();
        InitConfigurationDto.SDFS sdfs = sharedDataService.getInitConfigurationDto().getSdfs();

        String bucketName = sharedDataService.getInitConfigurationDto().getS3().getBucketName();
        String pathToExec = CreateAppConfigurationImpl.class.getResource("mount_sdfs.sh").getFile();
        String[] parameters = {sdfs.getVolumeSize(), amazonAWSAccessKey ,bucketName, amazonAWSSecretKey};
        try {
            Process p = Runtime.getRuntime().exec("." + pathToExec, parameters);
            p.waitFor();
            if (p.exitValue() != 0)
                throw new ConfigurationException("Error creating sdfs");
        }catch (IOException e) {
            //TODO: creation error handling
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private void storeAsWorkerConfiguration() {

    }

    private void dropDbTables() {

    }
}

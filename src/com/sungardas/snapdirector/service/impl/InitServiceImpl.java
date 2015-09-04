package com.sungardas.snapdirector.service.impl;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.model.*;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.sqs.AmazonSQSClient;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sungardas.init.CredentialsService;
import com.sungardas.snapdirector.dto.InitConfigurationDto;
import com.sungardas.snapdirector.exception.ConfigurationException;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.service.InitService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

@Service
public class InitServiceImpl implements InitService {
    private static final Logger LOG = LogManager.getLogger(InitServiceImpl.class);

    @Autowired private CredentialsService credentialsService;

    private Region currentRegion;
    private AmazonDynamoDB amazonDynamoDB;
    private AmazonS3 amazonS3;
    private InitConfigurationDto initConfigurationDto;

    @PostConstruct private void init() {
        currentRegion = Regions.getCurrentRegion();
        amazonDynamoDB = getDynamoDbClient();
        initConfigurationDto = getInitConfigurationDto();
    }

    private AmazonDynamoDBClient getDynamoDbClient() {
        return new AmazonDynamoDBClient(credentialsService);
    }

    private AmazonS3Client getS3client() {
        return new AmazonS3Client(credentialsService);
    }

    private AmazonSQSClient getSQSClient() {
        return new AmazonSQSClient(credentialsService);
    }

    private InitConfigurationDto getInitConfigurationDto() {
        ObjectMapper mapper = new ObjectMapper();
        String templateFile = "";
        try {
            return mapper.readValue( new File(templateFile), InitConfigurationDto.class);
        } catch (IOException cantReadConfigurationTemplate) {
            LOG.error("Can't read configuration from file{}", templateFile, cantReadConfigurationTemplate);
            throw new ConfigurationException(cantReadConfigurationTemplate);
        }
    }

    @Override
    public boolean isDbStructureValid() {
        String[] tables = {"BackupList", "Configurations", "Tasks", "Users", "Retention"};
        try {
            ListTablesResult listResult = amazonDynamoDB.listTables();
            List<String> tableNames = listResult.getTableNames();
            return containsAllOrAny(tables, tableNames);
        }catch (AmazonServiceException accessError) {
            LOG.info("Can't get a list of existed tables. Check AWS credentials!", accessError);
            throw new DataAccessException(accessError);
        }
    }

    private boolean containsAllOrAny(String[] toCheck, List<String> where) {
        if(where.size()==0) return true;
        int expectedCount = toCheck.length;
        int actualCount = 0;
        for(String value: toCheck) {
            if(where.contains(value)) actualCount++;
        }
        return expectedCount==actualCount;
    }

    @Override
    public void createDbStructure() throws ConfigurationException{
        createTable("BackupList", 1L, 1L, "volumeId", "S", "fileName ", "S");
        createTable("Configurations", 1L, 1L, "configurationId ", "S");
        createTable("Retention", 1L, 1L, "volumeId  ", "S");
        createTable("Schedule", 1L, 1L, "id  ", "S");
        createTable("Tasks", 1L, 1L, "id  ", "S");
        createTable("Users", 1L, 1L, "email   ", "S");
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

    private void isDefaultConfigurationValid() {
        initConfigurationDto.getQueue().getQueueName();
        initConfigurationDto.getS3().getBucketName();
        initConfigurationDto.getSdfs().getMountPoint();
        initConfigurationDto.getSdfs().getVolumeName();
    }

}

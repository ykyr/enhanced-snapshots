package com.sungardas.enhancedsnapshots.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.model.AttributeDefinition;
import com.amazonaws.services.dynamodbv2.model.CreateTableRequest;
import com.amazonaws.services.dynamodbv2.model.KeySchemaElement;
import com.amazonaws.services.dynamodbv2.model.KeyType;
import com.amazonaws.services.dynamodbv2.model.ListTablesResult;
import com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput;
import com.amazonaws.services.dynamodbv2.model.ResourceNotFoundException;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.model.CreateQueueRequest;
import com.amazonaws.util.EC2MetadataUtils;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;
import com.sungardas.enhancedsnapshots.dto.UserDto;
import com.sungardas.enhancedsnapshots.dto.converter.UserDtoConverter;
import com.sungardas.enhancedsnapshots.exception.ConfigurationException;
import com.sungardas.enhancedsnapshots.service.SDFSStateService;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import java.io.File;

class CreateAppConfigurationImpl {
    private static final Logger LOG = LogManager.getLogger(CreateAppConfigurationImpl.class);

    @Value("${amazon.s3.bucket}")
    private String s3Bucket;

    @Value("${amazon.sdfs.size}")
    private String sdfsSize;

    @Value("${amazon.aws.region}")
    private String region;

    @Autowired
    private SharedDataServiceImpl sharedDataService;

    @Autowired
    private SDFSStateService sdfsService;

    @Autowired
    private AmazonDynamoDB amazonDynamoDB;
    @Autowired
    private AmazonSQS amazonSQS;

    @Autowired
    private AmazonS3 amazonS3;

    private boolean init = false;

    @PostConstruct
    private void init() {
        if (!init) {
            LOG.info("Initialization started");
            init = true;
            InitConfigurationDto initConfigurationDto = sharedDataService.getInitConfigurationDto();
            if (initConfigurationDto == null) {
                sdfsService.startupSDFS(sdfsSize, s3Bucket);
                return;
            }

            boolean createDB = !initConfigurationDto.getDb().isValid();
            if (createDB) {
                LOG.info("Initialization DB");
                dropDbTables();
                createDbAndStoreData();
            } else {
                storeAdminUserIfProvided();
                if (!isConfigurationStored()) {
                    storeWorkerConfiguration();
                }
            }

            LOG.info("Initialization Queue");
            if (!initConfigurationDto.getQueue().isCreated()) {
                createTaskQueue();
            }

            boolean isBucketContainsSDFSMetadata = false;
            InitConfigurationDto.S3 s3 = initConfigurationDto.getS3().get(0);
            if (!isBucketExits(s3Bucket)) {
                LOG.info("Initialization S3 bucket");
                createS3Bucket();
            } else {
                isBucketContainsSDFSMetadata = sdfsService.containsSdfsMetadata(s3.getBucketName());
            }
            LOG.info("Initialization SDFS");
            if (isBucketContainsSDFSMetadata) {
                sdfsService.restoreState();
            } else {
                File sdfsConfig = new File("/etc/sdfs/awspool-volume-cfg.xml");
                if(sdfsConfig.exists()) sdfsConfig.delete();
                sdfsService.startupSDFS(sdfsSize, s3Bucket);
            }

            System.out.println(">>>Initialization finished");
            LOG.info("Initialization finished");
        }
    }

    private boolean isBucketExits(String s3Bucket) {
        try {
            String location = amazonS3.getBucketLocation(s3Bucket);
            return location != null;
        } catch (Exception e) {
            return false;
        }
    }

    private boolean isConfigurationStored() {
        InitConfigurationDto dto = sharedDataService.getInitConfigurationDto();
        WorkerConfiguration workerConfiguration = convertToWorkerConfiguration(dto);
        DynamoDBMapper mapper = new DynamoDBMapper(amazonDynamoDB);
        WorkerConfiguration loadedConf = mapper.load(WorkerConfiguration.class, workerConfiguration.getConfigurationId());
        return loadedConf != null;
    }

    private void createDbAndStoreData() {
        createDbStructure();
        storeAdminUserIfProvided();
        storeWorkerConfiguration();
    }

    private void createDbStructure() throws ConfigurationException {
        createTable("BackupList", 50L, 20L, "volumeId", "S", "fileName", "S");
        createTable("Configurations", 10L, 10L, "configurationId", "S");
        createTable("Retention", 50L, 20L, "volumeInstanceId", "S");
        createTable("Tasks", 50L, 20L, "id", "S");
        createTable("Snapshots", 50L, 20L, "volumeInstanceId", "S");
        createTable("Users", 50L, 20L, "id", "S");
        System.out.println(">> after createDbStructure");
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

            ArrayList<AttributeDefinition> attributeDefinitions = new ArrayList<>();
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
                    .withProvisionedThroughput(new ProvisionedThroughput()
                            .withReadCapacityUnits(readCapacityUnits)
                            .withWriteCapacityUnits(writeCapacityUnits));
            request.setAttributeDefinitions(attributeDefinitions);
            LOG.info("Issuing CreateTable request for " + tableName);
            Table table = dynamoDB.createTable(request);
            LOG.info("Waiting for " + tableName
                    + " to be created...this may take a while...");
            table.waitForActive();
        } catch (Exception e) {
            LOG.error("CreateTable request failed for " + tableName, e);
            throw new ConfigurationException("CreateTable request failed for " + tableName, e);
        }
    }

    private void storeAdminUserIfProvided() {
        UserDto userDto = sharedDataService.getAdminUser();
        String password = sharedDataService.getAdminPassword();
        if (userDto != null && password != null) {
            userDto.setEmail(userDto.getEmail().toLowerCase());
            User userToCreate = UserDtoConverter.convert(userDto);
            userToCreate.setPassword(DigestUtils.sha512Hex(password));
            userToCreate.setInstanceId(EC2MetadataUtils.getInstanceId());
            userToCreate.setRole("admin");
            DynamoDBMapper mapper = new DynamoDBMapper(amazonDynamoDB);
            mapper.save(userToCreate);
        }
    }

    private void createTaskQueue() {
        boolean deleteFirst = sharedDataService.getInitConfigurationDto().getQueue().isCreated();
        String queue = sharedDataService.getInitConfigurationDto().getQueue().getQueueName();
        queue = queue.substring(queue.lastIndexOf("/") + 1);
        if (deleteFirst) {
            amazonSQS.deleteQueue(queue);
            try {
                TimeUnit.SECONDS.sleep(65);
            } catch (InterruptedException e) {
                LOG.warn("Failed to delete queue [{}].", queue);
            }
        }
        CreateQueueRequest createQueueRequest = new CreateQueueRequest()
                .withQueueName(queue);
        amazonSQS.createQueue(createQueueRequest);
        LOG.info("Queue [{}] was created successfully.", queue);
    }

    private void createS3Bucket() {
        String bucketName = sharedDataService.getInitConfigurationDto().getS3().get(0).getBucketName();
        amazonS3.createBucket(bucketName);
        LOG.info("Bucket [{}] was created successfully.", bucketName);
    }

    private void storeWorkerConfiguration() {
        InitConfigurationDto dto = sharedDataService.getInitConfigurationDto();
        WorkerConfiguration workerConfiguration = convertToWorkerConfiguration(dto);
        DynamoDBMapper mapper = new DynamoDBMapper(amazonDynamoDB);
        mapper.save(workerConfiguration);
    }

    private WorkerConfiguration convertToWorkerConfiguration(InitConfigurationDto dto) {
        WorkerConfiguration workerConfiguration = new WorkerConfiguration();
        workerConfiguration.setConfigurationId(EC2MetadataUtils.getInstanceId());
        workerConfiguration.setEc2Region(Regions.getCurrentRegion().getName());
        workerConfiguration.setFakeBackupSource(null);
        workerConfiguration.setSdfsMountPoint(dto.getSdfs().getMountPoint());
        workerConfiguration.setSdfsVolumeName(dto.getSdfs().getVolumeName());
        workerConfiguration.setTaskQueueURL(dto.getQueue().getQueueName());
        workerConfiguration.setUseFakeBackup(false);
        workerConfiguration.setUseFakeEC2(false);
        workerConfiguration.setS3Bucket(sharedDataService.getInitConfigurationDto().getS3().get(0).getBucketName());
        return workerConfiguration;
    }

    private void dropDbTables() {
        DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
        ListTablesResult listResult = amazonDynamoDB.listTables();
        List<String> tableNames = listResult.getTableNames();
        String[] tables = {"BackupList", "Configurations", "Tasks", "Users", "Retention", "Snapshots"};
        for (String tableToDelete : tables) {
            if (tableNames.contains(tableToDelete)) {
                try {
                    Table table = dynamoDB.getTable(tableToDelete);
                    table.delete();
                    table.waitForDelete();
                    LOG.info("Table {} was removed successfully.", table);
                } catch (ResourceNotFoundException e) {
                    // Skip exception if resource not found
                } catch (AmazonServiceException tableNotFoundOrCredError) {
                    LOG.warn("Failed to remove table {}", tableToDelete);
                    throw new ConfigurationException("Can't delete tables. check AWS credentials");
                } catch (InterruptedException e) {
                    throw new ConfigurationException(e);
                }
            }
        }
    }
}

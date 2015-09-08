package com.sungardas.snapdirector.service.impl;


import com.amazonaws.AmazonServiceException;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.model.*;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.model.CreateQueueRequest;
import com.sungardas.snapdirector.aws.dynamodb.model.User;
import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.dto.InitConfigurationDto;
import com.sungardas.snapdirector.dto.UserDto;
import com.sungardas.snapdirector.dto.converter.UserDtoConverter;
import com.sungardas.snapdirector.exception.ConfigurationException;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.exception.SnapdirectorException;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.context.support.XmlWebApplicationContext;

import javax.annotation.PostConstruct;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.concurrent.TimeUnit;

class CreateAppConfigurationImpl {
    private static final Logger LOG = LogManager.getLogger(CreateAppConfigurationImpl.class);

    @Value("${amazon.aws.accesskey:}")
    private String amazonAWSAccessKey;
    @Value("${amazon.aws.secretkey}")
    private String amazonAWSSecretKey;

    @Value("${amazon.s3.bucket}")
    private String s3Bucket;

    @Value("${amazon.sdfs.size}")
    private String sdfsSize;

    @Autowired
    private SharedDataServiceImpl sharedDataService;

    @Autowired
    private AmazonDynamoDB amazonDynamoDB;
    @Autowired
    private AmazonSQS amazonSQS;

    @Autowired
    private XmlWebApplicationContext applicationContext;

    private boolean init = false;

    @PostConstruct
    private void init() {
        if (!init) {
            LOG.info("Initialization started");
            init = true;
            InitConfigurationDto initConfigurationDto = sharedDataService.getInitConfigurationDto();
            if (initConfigurationDto == null) {
                createSDFS(sdfsSize, s3Bucket);
                return;
            }

            boolean createDB = !initConfigurationDto.getDb().isValid();
            if (createDB) {
                LOG.info("Initialization DB");
                dropDbTables();
                createDbAndStoreData();
            } else {
                if(!isConfigurationStored()) {
                    storeWorkerConfiguration();
                }
            }

            LOG.info("Initialization Queue");
            if(!initConfigurationDto.getQueue().isCreated()) {
                createTaskQueue();
            }

            if (!initConfigurationDto.getSdfs().isCreated()) {
                LOG.info("Initialization SDFS");
                createSDFS();
            }
            System.out.println(">>>Initialization finished");
            LOG.info("Initialization finished");
        }
    }

    private boolean isConfigurationStored() {
        InitConfigurationDto dto = sharedDataService.getInitConfigurationDto();
        WorkerConfiguration workerConfiguration = convertToWorkerConfiguration(dto);
        DynamoDBMapper mapper = new DynamoDBMapper(amazonDynamoDB);
        WorkerConfiguration loadedConf = mapper.load(WorkerConfiguration.class, workerConfiguration.getConfigurationId());
        return loadedConf!=null;
    }

    private void createDbAndStoreData() {
        createDbStructure();
        storeAdminUserIfProvided();
        storeWorkerConfiguration();
    }

    private void createDbStructure() throws ConfigurationException {
        createTable("BackupList", 50L, 20L, "volumeId", "S", "fileName", "S");
        createTable("Configurations", 10L, 10L, "configurationId", "S");
        createTable("Retention", 50L, 20L, "volumeId", "S");
        createTable("Tasks", 50L, 20L, "id", "S");
        createTable("Users", 50L, 20L, "email", "S");
        createTable("Snapshots", 50L, 20L, "id", "S");
        System.out.println(">> after createDbStructure");
    }

    private boolean isDbExists() {
        String[] tables = {"BackupList", "Configurations", "Tasks", "Users", "Retention", "Snapshots"};
        try {
            ListTablesResult listResult = amazonDynamoDB.listTables();
            List<String> tableNames = listResult.getTableNames();
            return tableNames.containsAll(Arrays.asList(tables));
        } catch (AmazonServiceException accessError) {
            LOG.info("Can't get a list of existed tables. Check AWS credentials!", accessError);
            throw new DataAccessException(accessError);
        }
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
            User userToCreate = UserDtoConverter.convert(userDto);
            userToCreate.setPassword(DigestUtils.sha512Hex(password));
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
            }
        }

        CreateQueueRequest createQueueRequest = new CreateQueueRequest()
                .withQueueName(queue);
        amazonSQS.createQueue(createQueueRequest);
    }

    private void createSDFS() {
        InitConfigurationDto.SDFS sdfs = sharedDataService.getInitConfigurationDto().getSdfs();
        String bucketName = sharedDataService.getInitConfigurationDto().getS3().getBucketName();

        createSDFS(sdfs.getVolumeSize(), bucketName);
    }

    private void createSDFS(String size, String bucketName) {
        try {
            File file = applicationContext.getResource("classpath:sdfs1.sh").getFile();
            file.setExecutable(true);
            String pathToExec = file.getAbsolutePath();
            String[] parameters = {pathToExec, amazonAWSAccessKey, amazonAWSSecretKey, size, bucketName};
            Process p = Runtime.getRuntime().exec(parameters);
            p.waitFor();
            print(p);
            switch (p.exitValue()) {
                case 0:
                    LOG.info("SDFS mounted");
                    break;
                case 1:
                    LOG.info("SDFS unmounted");
                    p = Runtime.getRuntime().exec(parameters);
                    p.waitFor();
                    print(p);
                    if (p.exitValue() != 0) {
                        throw new ConfigurationException("Error creating sdfs");
                    }
                    LOG.info("SDFS mounted");
                    break;
                default:
                    print(p);
                    throw new ConfigurationException("Error creating sdfs");
            }
        } catch (IOException e) {
            LOG.error(e);
        } catch (InterruptedException e) {
            LOG.error(e);
        }
    }

    private void print(Process p) throws IOException {
        String line;
        BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
        while ((line = input.readLine()) != null) {
            System.out.println(line);
        }
        input = new BufferedReader(new InputStreamReader(p.getErrorStream()));
        while ((line = input.readLine()) != null) {
            System.out.println(line);
        }
    }

    private void storeWorkerConfiguration() {
        InitConfigurationDto dto = sharedDataService.getInitConfigurationDto();
        WorkerConfiguration workerConfiguration = convertToWorkerConfiguration(dto);
        DynamoDBMapper mapper = new DynamoDBMapper(amazonDynamoDB);
        mapper.save(workerConfiguration);
    }

    private WorkerConfiguration convertToWorkerConfiguration(InitConfigurationDto dto) {
        WorkerConfiguration workerConfiguration = new WorkerConfiguration();
        workerConfiguration.setConfigurationId(getInstanceId());
        workerConfiguration.setEc2Region(Regions.getCurrentRegion().getName());
        workerConfiguration.setFakeBackupSource(null);
        workerConfiguration.setSdfsMountPoint(dto.getSdfs().getMountPoint());
        workerConfiguration.setSdfsVolumeName(dto.getSdfs().getVolumeName());
        workerConfiguration.setTaskQueueURL(dto.getQueue().getQueueName());
        workerConfiguration.setUseFakeBackup(false);
        workerConfiguration.setUseFakeEC2(false);
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
                } catch (AmazonServiceException tableNotFoundOrCredError) {
                    throw new ConfigurationException("Can't delete tables. check AWS credentials");
                } catch (InterruptedException e) {
                    throw new ConfigurationException(e);
                }
            }
        }
    }

    private String getInstanceId() {
        String instanceId = null;
        try {
            URL url = new URL("http://169.254.169.254/latest/meta-data/instance-id");
            URLConnection conn = url.openConnection();
            Scanner s = new Scanner(conn.getInputStream());
            if (s.hasNext()) {
                instanceId = s.next();
                LOG.info("Getting configuration id from metadata: " + instanceId);
            }
            s.close();
        } catch (IOException e) {
            LOG.warn("Failed to determine ec2 instance ID");
            throw new SnapdirectorException("Failed to determine ec2 instance ID", e);
        }
        return instanceId;
    }
}

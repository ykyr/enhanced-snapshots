package com.sungardas.snapdirector.service.impl;

import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.model.*;
import com.amazonaws.services.ec2.AmazonEC2Client;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.model.ListQueuesResult;
import com.sungardas.snapdirector.aws.PropertyBasedCredentialsProvider;
import com.sungardas.snapdirector.aws.dynamodb.Roles;
import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.aws.dynamodb.repository.UserRepository;
import com.sungardas.snapdirector.aws.dynamodb.repository.WorkerConfigurationRepository;
import com.sungardas.snapdirector.exception.ConfigurationException;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.exception.SnapdirectorException;
import com.sungardas.snapdirector.service.InitializationService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.ObjectFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

@Service
@Profile("prod")
public class InitializationServiceImpl implements InitializationService {
    public final Logger LOG = LogManager.getLogger(InitializationServiceImpl.class);

    private static String DEFAULT_SDFS_CONFIG_LOCATION = "/etc/sdfs";
    private static String DEFAULT_VOLUME_CONFIG_FILE_SUFFIX = "-";

    private static String DEFAULT_LOGIN = "admin";
    private String[] tables = {"BackupList", "Configurations", "Tasks", "Users", "Retention"};

    @Autowired
    private AmazonSQS sqs;
    @Autowired
    private AmazonDynamoDB amazonDynamoDB;

    @Autowired
    private ObjectFactory<UserRepository> userRepositoryObjectFactory;
    private UserRepository userRepository;

    @Autowired
    private ObjectFactory<WorkerConfigurationRepository> workerConfigurationRepositoryObjectFactory;
    private WorkerConfigurationRepository configurationRepository;
    private WorkerConfiguration currentConfiguration;

    private boolean dbStructureValid = false;
    private boolean configurationExists = false;
    private boolean queueExists = false;
    private boolean adminUserExists = false;

    @PostConstruct
    private void init() {
        if(isDbStructureValid()) {
            userRepository = userRepositoryObjectFactory.getObject();
            configurationRepository = workerConfigurationRepositoryObjectFactory.getObject();
            currentConfiguration = configurationRepository.findOne(getConfigurationId());
        }

    }

    @Override
    public boolean isSystemInitialized() {
        boolean isOk= true;

        isOk = isOk && isDbStructureValid();
        isOk = isOk && isConfigurationExists();
        isOk = isOk && isQueueExists();
        isOk = isOk && isSdfsConfigured();
        isOk = isOk && isAdminUserExists();

        return isOk;
    }


    @Override
    public boolean isDbStructureValid() {
        if(!dbStructureValid) {
            dbStructureValid = checkDbStructureIsValid();
        }
        return dbStructureValid;
    }

    @Override
    public boolean isConfigurationExists() {
        if(!configurationExists) {
            configurationExists = checkConfigurationExists();
        }
        return configurationExists;
    }

    @Override
    public boolean isQueueExists() {
        if(!queueExists) {
            queueExists = checkQueueExists();
        }
        return queueExists;
    }


    @Override
    public boolean isSdfsConfigured() {
        Path sdfsConfPath = Paths.get(DEFAULT_SDFS_CONFIG_LOCATION);
        boolean configPathExists = Files.exists(sdfsConfPath) && Files.isDirectory(sdfsConfPath);
        return true;
    }

    @Override
    public boolean isAdminUserExists() {
        if(!adminUserExists) {
            adminUserExists = userRepository.findByRole(Roles.ADMIN.getName()).size()>0;
        }
        return  adminUserExists;
    }


    private boolean checkDbStructureIsValid() {
        String[] tables = {"BackupList", "Configurations", "Tasks", "Users", "Retention"};
        boolean isValid;

        isValid = false;
        try {
            ListTablesResult listResult = amazonDynamoDB.listTables();
            List<String> tableNames = listResult.getTableNames();
            isValid = tableNames.retainAll(Arrays.asList(tables));
        }catch (AmazonServiceException accessError) {
            DataAccessException dae =  new DataAccessException("Can't get a list of existed tables. Check AWS credentials!",accessError);
            LOG.info(dae);
        }
        return isValid;
    }

    private void createDbStructure() {
        createTable("BackupList", 1L, 1L, "volumeId", "S", "fileName ", "S");
        createTable("Configurations", 1L, 1L, "configurationId ", "S");
        createTable("Retention", 1L, 1L, "volumeId  ", "S");
        createTable("Schedule", 1L, 1L, "id  ", "S");
        createTable("Tasks", 1L, 1L, "id  ", "S");
        createTable("Users", 1L, 1L, "email   ", "S");
    }

    public void createConfiguration(WorkerConfiguration configuration ) {
        configurationRepository.save(configuration);
    }

    public void createResources(WorkerConfiguration configuration ) {

    }

    private boolean checkConfigurationExists() {
        if(!dbStructureValid)
            throw new ConfigurationException("Can't check configuration. DB structure is missing ");

        String configId = getConfigurationId();
        WorkerConfiguration conf = configurationRepository.findOne(configId);
        boolean isCorrectConfiguration = true;
        if (conf != null) {

            LOG.info("Configuration {} exists", configId);
            if (conf.getEc2Region()!=null && !conf.getEc2Region().equals("")) {
                LOG.info("Configuration {} doesn't have correct region.",configId);
                isCorrectConfiguration = false;
            }

            if (conf.getTaskQueueURL()!=null && !conf.getTaskQueueURL().equals("")) {
                LOG.info("Configuration {} doesn't contains correct SQS queue name.",configId);
                isCorrectConfiguration = false;
            }

            if (conf.getSdfsMountPoint()!=null && !conf.getSdfsMountPoint().equals("")) {
                LOG.info("Configuration {} doesn't contains correct mount point.",configId);
                isCorrectConfiguration = false;
            }

            if (conf.getSdfsVolumeName()!=null && !conf.getSdfsVolumeName().equals("")) {
                LOG.info("Configuration {} doesn't contains correct volume name.",configId);
                isCorrectConfiguration = false;
            }

            if(isCorrectConfiguration) currentConfiguration = conf;
        }
        else {
            isCorrectConfiguration = false;
        }
        return isCorrectConfiguration;
    }

    private boolean checkQueueExists() {
        if(currentConfiguration==null) {
            throw new ConfigurationException("Can't check SQS queue. No configuration loaded");
        }

        String queueName = currentConfiguration.getTaskQueueURL();
        ListQueuesResult lqResult = sqs.listQueues();
        return lqResult.getQueueUrls().contains(queueName);
    }

    protected String getConfigurationId() {
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

            ArrayList<KeySchemaElement> keySchema = new ArrayList<KeySchemaElement>();
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
}

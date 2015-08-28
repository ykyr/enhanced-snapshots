package com.sungardas.snapdirector.service.impl;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.model.ListTablesResult;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.model.ListQueuesResult;
import com.sungardas.snapdirector.aws.dynamodb.Roles;
import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.aws.dynamodb.repository.UserRepository;
import com.sungardas.snapdirector.aws.dynamodb.repository.WorkerConfigurationRepository;
import com.sungardas.snapdirector.exception.ConfigurationException;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.service.InitializationService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

@Service
@Profile("prod")
public class InitializationServiceImpl implements InitializationService {
    public static final Logger LOG = LogManager.getLogger(InitializationServiceImpl.class);

    @Autowired
    private AmazonSQS sqs;
    @Autowired
    private AmazonDynamoDB amazonDynamoDB;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private WorkerConfigurationRepository configurationRepository;

    private WorkerConfiguration currentConfiguration;

    private boolean dbStructureValid = false;
    private boolean configurationExists = false;
    private boolean queueExists = false;
    private boolean adminUserExists = false;


    @Override
    public boolean AWSCredentialsAreValid(String accessKey, String secretKey) {
        AmazonDynamoDB client = new AmazonDynamoDBClient(new BasicAWSCredentials(accessKey, secretKey));
        try{
            client.listTables();
        }catch (AmazonServiceException accessError) {
            LOG.info("AWS Credentials are invalid!");
            return false;
        }
        return true;

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
    public boolean checkDefaultUser(String login, String passwd) {
        return true;
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

    private boolean checkConfigurationExists() {
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
            LOG.error("Cant get configuration id from metadata!");
            throw new ConfigurationException("Cant get Instance Id from metadata!");
        }
        return instanceId;
    }
}

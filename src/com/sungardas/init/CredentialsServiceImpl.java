package com.sungardas.init;

import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBScanExpression;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.Condition;
import com.amazonaws.services.dynamodbv2.model.ListTablesResult;
import com.amazonaws.services.ec2.AmazonEC2Client;
import com.amazonaws.services.identitymanagement.AmazonIdentityManagementClient;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.AmazonS3Exception;
import com.amazonaws.services.s3.model.Bucket;
import com.amazonaws.services.s3.model.ListObjectsRequest;
import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.util.EC2MetadataUtils;
import com.sun.management.UnixOperatingSystemMXBean;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;
import com.sungardas.enhancedsnapshots.exception.ConfigurationException;
import com.sungardas.enhancedsnapshots.exception.DataAccessException;
import com.sungardas.enhancedsnapshots.exception.EnhancedSnapshotsException;
import com.sungardas.enhancedsnapshots.service.CryptoService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import javax.validation.constraints.NotNull;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import static com.amazonaws.services.dynamodbv2.model.ComparisonOperator.EQ;

@Service
class CredentialsServiceImpl implements CredentialsService {

    private static String AWS_ACCESS_KEY_ID;
    private static String AWS_SECRET_ACCESS_KEY;
    private static final String NOT_ENOUGH_MEMORY_ERROR = "Current instance doesn't  provide enough memory to start SDFS. At least 3.75GB  of total memory expected.";
    private static final String CANT_GET_ACCESS_DYNAMODB = "Can't get access to DynamoDB. Check policy list used for AWS user";
    private static final String CANT_GET_ACCESS_SQS = "Can't get access to SQS. Check policy list used for AWS user";
    private static final String CANT_GET_INSTANCE_ID = "Can't get instance ID from metadata . Check policy list used for AWS user";
    private static final String INVALID_CREDS = "Invalid AWS credentials! The instance should be terminated. Please, provide correct AccessKeyID and SecretKey for a new Instance.";
    private static final String CANT_GET_ACCESS_S3 = "Can't get access to S3. Check policy list used for AWS user";

    private final String catalinaHomeEnvPropName = "catalina.home";
    private final String confFolderName = "conf";
    private final String propFileName = "amazon.properties";
    private final String accessKeyPropName = "amazon.aws.accesskey";
    private final String secretKeyPropName = "amazon.aws.secretkey";
    private static final String AMAZON_S3_BUCKET = "amazon.s3.bucket";
    private static final String AMAZON_SDFS_SIZE = "amazon.sdfs.size";
    private static final String AMAZON_AWS_REGION = "amazon.aws.region";
    private static final String SUNGARGAS_WORKER_CONFIGURATION = "sungardas.worker.configuration";
    private static final Logger LOG = LogManager.getLogger(CredentialsServiceImpl.class);
    private static final long BYTES_IN_GB = 1_073_741_824;
    private AWSCredentials credentials = null;
    private final String DEFAULT_LOGIN = "admin@enhancedsnapshots";
    private String instanceId;

    @Value("${enhancedsnapshots.sdfs.default.size}")
    private String defaultVolumeSize;

    @Value("${enhancedsnapshots.db.tables}")
    private String[] tables;

    private InitConfigurationDto initConfigurationDto = null;


    @Autowired
    private CryptoService cryptoService;

    @PostConstruct
    private void init() {
        AWS_ACCESS_KEY_ID = System.getenv().get("AWS_ACCESS_KEY_ID");
        AWS_SECRET_ACCESS_KEY = System.getenv("AWS_SECRET_ACCESS_KEY");
        if (AWS_ACCESS_KEY_ID != null && AWS_SECRET_ACCESS_KEY != null) {
            credentials = new BasicAWSCredentials(AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY);
        }
        instanceId = EC2MetadataUtils.getInstanceId();
    }


    @Override
    public void setCredentialsIfValid(@NotNull CredentialsDto credentials) {
        validateCredentials(credentials.getAwsPublicKey(), credentials.getAwsSecretKey());
        this.credentials = new BasicAWSCredentials(credentials.getAwsPublicKey(), credentials.getAwsSecretKey());
    }

    @Override
    public void storeCredentials() {
        validateCredentials(credentials.getAWSAccessKeyId(), credentials.getAWSSecretKey());
        Properties properties = new Properties();
        File file = Paths.get(System.getProperty(catalinaHomeEnvPropName), confFolderName, propFileName).toFile();
        try {

            properties.setProperty(accessKeyPropName, cryptoService.encrypt(instanceId, credentials.getAWSAccessKeyId()));
            properties.setProperty(secretKeyPropName, cryptoService.encrypt(instanceId, credentials.getAWSSecretKey()));
            properties.setProperty(AMAZON_AWS_REGION, Regions.getCurrentRegion().getName());
            properties.setProperty(SUNGARGAS_WORKER_CONFIGURATION, instanceId);
            properties.setProperty(AMAZON_S3_BUCKET, initConfigurationDto.getS3().get(0).getBucketName());
            properties.setProperty(AMAZON_SDFS_SIZE, initConfigurationDto.getSdfs().getVolumeSize());
            properties.store(new FileOutputStream(file), "AWS Credentials");
        } catch (IOException ioException) {
            throw new ConfigurationException("Can not create amazon.properties file\n" +
                    "Check path or permission: " + file.getAbsolutePath(), ioException);
        }
    }

    @Override
    public void removeCredentials() {
        File file = Paths.get(System.getProperty(catalinaHomeEnvPropName), confFolderName, propFileName).toFile();
        if (file.exists()) {
            file.delete();
        }
    }

    @Override
    public boolean areCredentialsValid() {
        AmazonEC2Client ec2Client = new AmazonEC2Client(credentials);
        try {
            ec2Client.describeRegions();
            return true;
        } catch (AmazonClientException e) {
            LOG.warn("Provided AWS credentials are invalid.");
            return false;
        }
    }

    @Override
    public boolean credentialsAreProvided() {
       if(credentials!=null) {
           validateCredentials(credentials.getAWSAccessKeyId(), credentials.getAWSSecretKey());
           return true;
       } else {
           return false;
       }

    }

    @Override
    public boolean isAwsPropertyFileExists() {
        return getPropertyFile().exists();
    }

    @Override
    public boolean checkDefaultUser(String login, String password) {
        return DEFAULT_LOGIN.equals(login.toLowerCase()) && password.equals(instanceId);
    }

    private List<InitConfigurationDto.S3> getBucketsWithSdfsMetadata() {
        ArrayList<InitConfigurationDto.S3> result = new ArrayList<>();
        try {
            AmazonS3Client client = new AmazonS3Client(credentials);
            List<Bucket> allBuckets = client.listBuckets();
            String bucketName = "com.sungardas.enhancedsnapshots." + instanceId;
            result.add(new InitConfigurationDto.S3(bucketName, false));

            String currentLocation = Regions.getCurrentRegion().toString();
            if (currentLocation.equalsIgnoreCase("us-east-1")) currentLocation = "US";
            for (Bucket bucket : allBuckets) {
                try {
                    ListObjectsRequest request = new ListObjectsRequest()
                            .withBucketName(bucket.getName()).withPrefix("sdfsstate");
                    if (client.listObjects(request).getObjectSummaries().size() > 0) {
                        if (bucketName.equals(bucket.getName())) {
                            result.get(0).setCreated(true);
                        } else {
                            String location = client.getBucketLocation(bucket.getName());

                            if (!location.equalsIgnoreCase(currentLocation))
                                continue;
                            result.add(new InitConfigurationDto.S3(bucket.getName(), true));
                        }
                    }
                } catch (AmazonS3Exception ignored) {
                }
            }
        } catch (AmazonS3Exception e) {
            LOG.warn("Can't get access to S3");
            throw new DataAccessException(CANT_GET_ACCESS_S3, e);
        }
        return result;

    }

    @Override
    public InitConfigurationDto getInitConfigurationDto() {
        initConfigurationDto = new InitConfigurationDto();
        initConfigurationDto.setDb(new InitConfigurationDto.DB());
        boolean isDbValid = requiredTablesExist();
        initConfigurationDto.getDb().setValid(isDbValid);
        if (isDbValid) {
            initConfigurationDto.getDb().setAdminExist(adminExist());
        }

        initConfigurationDto.setS3(getBucketsWithSdfsMetadata());

        String queueName = getAccountId() + "/enhancedsnapshots_" + instanceId;
        InitConfigurationDto.Queue queue = new InitConfigurationDto.Queue();
        queue.setQueueName(queueName);
        queue.setCreated(queueAlreadyExists(queueName));

        String volumeName = "awspool";
        String mountPoint = "/mnt/awspool/";
        InitConfigurationDto.SDFS sdfs = new InitConfigurationDto.SDFS();
        sdfs.setMountPoint(mountPoint);
        sdfs.setVolumeName(volumeName);
        sdfs.setVolumeSize(volumeSize());
        sdfs.setCreated(sdfsAlreadyExists(volumeName, mountPoint));

        initConfigurationDto.setS3(getBucketsWithSdfsMetadata());
        initConfigurationDto.setQueue(queue);
        initConfigurationDto.setSdfs(sdfs);
        return initConfigurationDto;
    }

    private String volumeSize() {
        freeMemCheck();
        return defaultVolumeSize;
    }

    private void freeMemCheck() {
        UnixOperatingSystemMXBean osBean = (UnixOperatingSystemMXBean) ManagementFactory.getOperatingSystemMXBean();
        long total = osBean.getTotalPhysicalMemorySize();
        long required = (long) (3.5 * BYTES_IN_GB);
        if (total < required) {
            LOG.error("Total memory {}. Required memory {}", total, required);
            throw new EnhancedSnapshotsException(NOT_ENOUGH_MEMORY_ERROR);
        }
    }

    private boolean requiredTablesExist() {
        AmazonDynamoDBClient amazonDynamoDB = new AmazonDynamoDBClient(credentials);
        amazonDynamoDB.setRegion(Regions.getCurrentRegion());
        try {
            ListTablesResult listResult = amazonDynamoDB.listTables();
            List<String> tableNames = listResult.getTableNames();
            LOG.info("List db structure: {}", tableNames.toArray());
            LOG.info("Check db structure is present: {}", tableNames.containsAll(Arrays.asList(tables)));
            return tableNames.containsAll(Arrays.asList(tables));
        } catch (AmazonServiceException e) {
            LOG.warn("Can't get a list of existed tables", e);
            throw new DataAccessException(CANT_GET_ACCESS_DYNAMODB, e);
        }
    }

    private boolean adminExist() {
        AmazonDynamoDBClient client = new AmazonDynamoDBClient(credentials);
        client.setRegion(Regions.getCurrentRegion());
        DynamoDBMapper mapper = new DynamoDBMapper(client);
        DynamoDBScanExpression expression = new DynamoDBScanExpression()
                .withFilterConditionEntry("role",
                        new Condition().withComparisonOperator(EQ.toString()).withAttributeValueList(new AttributeValue("admin")))
                .withFilterConditionEntry("instanceId",
                        new Condition().withComparisonOperator(EQ.toString()).withAttributeValueList(new AttributeValue(instanceId)));
        List<User> users = mapper.scan(User.class, expression);

        return !users.isEmpty();
    }


    private boolean queueAlreadyExists(String queueName) {
        AmazonSQSClient amazonSQSClient = new AmazonSQSClient(credentials);
        amazonSQSClient.setRegion(Regions.getCurrentRegion());
        try {
            for (String s : amazonSQSClient.listQueues().getQueueUrls()) {
                if (s.contains(queueName)) return true;
            }
            return false;
        } catch (AmazonServiceException accessError) {
            LOG.info("Can't get a list of queues. Check AWS credentials!", accessError);
            throw new DataAccessException(CANT_GET_ACCESS_SQS, accessError);
        }
    }

    private boolean sdfsAlreadyExists(String volumeName, String mountPoint) {
        String volumeConfigPath = "/etc/sdfs/" + volumeName + "-volume-cfg.xml";
        File configf = new File(volumeConfigPath);
        File mountPointf = new File(mountPoint);
        return configf.exists() && mountPointf.exists();
    }

    private void validateCredentials(String accessKey, String secretKey) {
        if (accessKey == null || accessKey.isEmpty()) {
            throw new ConfigurationException("Empty AWS AccessKey");
        }
        if (secretKey == null || secretKey.isEmpty()) {
            throw new ConfigurationException("Empty AWS SecretKey");
        }
        try {
            AmazonEC2Client ec2Client = new AmazonEC2Client(new BasicAWSCredentials(accessKey, secretKey));
            ec2Client.describeRegions();
        } catch (AmazonClientException e) {
            throw new ConfigurationException(INVALID_CREDS, e);
        }
    }

    private File getPropertyFile() {
        return Paths.get(System.getProperty(catalinaHomeEnvPropName), confFolderName, propFileName).toFile();
    }

    private String getAccountId() {
        AmazonIdentityManagementClient iamClient = new AmazonIdentityManagementClient(credentials);
        try {
            return iamClient.getUser().getUser().getArn().replaceAll("[^\\d]", "");
        } catch (AmazonServiceException accessError) {
            LOG.info("Can't get userId. Check AWS credentials!", accessError);
            throw new DataAccessException(CANT_GET_INSTANCE_ID, accessError);
        }
    }

    @Override
    public String getInstanceId() {
        return instanceId;
    }
}

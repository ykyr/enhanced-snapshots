package com.sungardas.init;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.annotation.PostConstruct;
import javax.validation.constraints.NotNull;

import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.auth.InstanceProfileCredentialsProvider;
import com.amazonaws.internal.StaticCredentialsProvider;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBScanExpression;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.model.*;
import com.amazonaws.services.ec2.AmazonEC2Client;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.*;
import com.amazonaws.util.EC2MetadataUtils;
import com.sun.management.UnixOperatingSystemMXBean;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.Configuration;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;
import com.sungardas.enhancedsnapshots.dto.UserDto;
import com.sungardas.enhancedsnapshots.dto.converter.UserDtoConverter;
import com.sungardas.enhancedsnapshots.exception.ConfigurationException;
import com.sungardas.enhancedsnapshots.exception.DataAccessException;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.configuration2.PropertiesConfiguration;
import org.apache.commons.configuration2.PropertiesConfigurationLayout;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.support.CronSequenceGenerator;
import org.springframework.stereotype.Service;

import static com.amazonaws.services.dynamodbv2.model.ComparisonOperator.EQ;

@Service
class InitConfigurationServiceImpl implements InitConfigurationService {

    private static final Logger LOG = LogManager.getLogger(InitConfigurationServiceImpl.class);
    private static final long BYTES_IN_GB = 1_073_741_824;
    private static final int SDFS_VOLUME_SIZE_IN_GB_PER_GB_OF_RAM = 2000;
    private static final long SYSTEM_RESERVED_RAM_IN_BYTES = BYTES_IN_GB;
    private static final long SDFS_RESERVED_RAM_IN_BYTES = BYTES_IN_GB;

    private static final String catalinaHomeEnvPropName = "catalina.home";
    private static final String confFolderName = "conf";
    private static final String propFileName = "EnhancedSnapshots.properties";
    private static final String DEFAULT_LOGIN = "admin@enhancedsnapshots";

    private static final String ENHANCED_SNAPSHOT_BUCKET_PREFIX = "com.sungardas.enhancedsnapshots.";
    private static final String CANT_GET_ACCESS_DYNAMODB = "Can't get access to DynamoDB. Check policy list used for AWS user";
    private static final String CANT_GET_ACCESS_S3 = "Can't get access to S3. Check policy list used for AWS user";

    // properties to store in config file
    private static final String WORKER_POLLING_RATE = "enhancedsnapshots.polling.rate";
    private static final String RETENTION_CRON_SCHEDULE = "enhancedsnapshots.retention.cron";
    private static final String WAIT_TIME_BEFORE_NEXT_CHECK_IN_SECONDS = "enhancedsnapshots.wait.time.before.new.sync";
    private static final String MAX_WAIT_TIME_VOLUME_TO_DETACH_IN_SECONDS = "enhancedsnapshots.max.wait.time.to.detach.volume";

    // properties from $catalina.home/conf/EnhancedSnapshots.properties config file
    @Value("${enhancedsnapshots.retention.cron: -1}")
    private String retentionCronExpression;
    @Value("${enhancedsnapshots.polling.rate: -1}")
    private int pollingRate;
    @Value("${enhancedsnapshots.wait.time.before.new.sync: -1}")
    private int waitTimeBeforeNewSync;
    @Value("${enhancedsnapshots.max.wait.time.to.detach.volume: -1}")
    private int maxWaitTimeToDetachVolume;
    private int propertyNotProvided = -1;

    @Value("${enhancedsnapshots.sdfs.min.size}")
    private String minVolumeSize;

    // default application properties
    @Value("${enhancedsnapshots.default.tempVolumeType}")
    private String tempVolumeType;
    @Value("${enhancedsnapshots.default.tempVolumeIopsPerGb}")
    private int tempVolumeIopsPerGb;
    @Value("${enhancedsnapshots.default.restoreVolumeType}")
    private String restoreVolumeType;
    @Value("${enhancedsnapshots.default.restoreVolumeIopsPerGb}")
    private int restoreVolumeIopsPerGb;
    @Value("${enhancedsnapshots.default.amazon.retry.count}")
    private int amazonRetryCount;
    @Value("${enhancedsnapshots.default.amazon.retry.sleep}")
    private int amazonRetrySleep;
    @Value("${enhancedsnapshots.default.queue.size}")
    private int queueSize;
    @Value("${enhancedsnapshots.default.sdfs.volume.config.path}")
    private String sdfsConfigPath;
    @Value("${enhancedsnapshots.default.sdfs.backup.file.name}")
    private String sdfsStateBackupFileName;
    @Value("${enhancedsnapshots.default.retention.cron}")
    private String defaultRetentionCronExpression;
    @Value("${enhancedsnapshots.default.polling.rate}")
    private int defaultPollingRate;
    @Value("${enhancedsnapshots.default.sdfs.local.cache.size}")
    private String sdfsLocalCacheSize;
    @Value("${enhancedsnapshots.default.wait.time.before.new.sync}")
    private int defaultWaitTimeBeforeNewSyncWithAWS;
    @Value("${enhancedsnapshots.default.max.wait.time.to.detach.volume}")
    private int defaultMaxWaitTimeToDetachVolume;
    @Value("${enhancedsnapshots.default.sdfs.size}")
    private String defaultVolumeSize;
    @Value("${enhancedsnapshots.db.tables}")
    private String[] tables;
    @Value("${amazon.s3.default.region}")
    private String defaultS3Region;
    @Value("${enhancedsnapshots.default.sdfs.mount.point}")
    private String mountPoint;
    @Value("${enhancedsnapshots.default.sdfs.volume.name}")
    private String volumeName;
    @Value("${enhancedsnapshots.awscli.conf.path}")
    private String awscliConfPath;
    @Value("${enhancedsnapshots.awslogs.conf.path}")
    private String awslogsConfPath;

    private AWSCredentialsProvider credentialsProvider;
    private AmazonDynamoDB amazonDynamoDB;
    private DynamoDBMapper mapper;
    private InitConfigurationDto initConfigurationDto;
    private String instanceId;
    private Region region;

    private UserDto userDto;
    private String adminPassword;

    @PostConstruct
    private void init() {
        credentialsProvider = new InstanceProfileCredentialsProvider();
        instanceId = EC2MetadataUtils.getInstanceId();
        region = Regions.getCurrentRegion();
        amazonDynamoDB = new AmazonDynamoDBClient(credentialsProvider);
        amazonDynamoDB.setRegion(region);

        mapper = new DynamoDBMapper(amazonDynamoDB);
    }

    @Override
    public void setUser(User user) {
        if (user != null) {
            userDto = UserDtoConverter.convert(user);
            userDto.setRole("admin");
            adminPassword = user.getPassword();
        }
    }

    @Override
    public void setInitConfigurationDto(InitConfigurationDto initConfigurationDto) {
        this.initConfigurationDto = initConfigurationDto;
    }

    @Override
    public void setCredentialsIfValid(@NotNull CredentialsDto credentials) {
        validateCredentials(credentials.getAwsPublicKey(), credentials.getAwsSecretKey());
        credentialsProvider = new StaticCredentialsProvider(new BasicAWSCredentials(credentials.getAwsPublicKey(), credentials.getAwsSecretKey()));
    }


    /**
     *  Stores properties which can not be modified from UI to config file,
     *  changes in config file will be applied after system restart
     */
    public void storePropertiesEditableFromConfigFile() {
        File file = Paths.get(System.getProperty(catalinaHomeEnvPropName), confFolderName, propFileName).toFile();
        try {
            PropertiesConfiguration propertiesConfiguration = new PropertiesConfiguration();
            PropertiesConfigurationLayout layout = propertiesConfiguration.getLayout();
            layout.setLineSeparator("\n");
            String headerComment = "EnhancedSnapshots properties. Changes in this config file will be applied after application restart.";

            layout.setHeaderComment(headerComment);

            layout.setBlancLinesBefore(RETENTION_CRON_SCHEDULE, 1);
            layout.setComment(RETENTION_CRON_SCHEDULE, "Cron schedule for retention policy");
            propertiesConfiguration.setProperty(RETENTION_CRON_SCHEDULE, defaultRetentionCronExpression);

            layout.setBlancLinesBefore(WORKER_POLLING_RATE, 1);
            layout.setComment(WORKER_POLLING_RATE, "Polling rate to check whether there is some new task, in ms");
            propertiesConfiguration.setProperty(WORKER_POLLING_RATE, Integer.toString(defaultPollingRate));

            layout.setBlancLinesBefore(WAIT_TIME_BEFORE_NEXT_CHECK_IN_SECONDS, 1);
            layout.setComment(WAIT_TIME_BEFORE_NEXT_CHECK_IN_SECONDS, "Wait time before new sync of Snapshot/Volume with AWS data, in seconds");
            propertiesConfiguration.setProperty(WAIT_TIME_BEFORE_NEXT_CHECK_IN_SECONDS, Integer.toString(defaultWaitTimeBeforeNewSyncWithAWS));

            layout.setBlancLinesBefore(MAX_WAIT_TIME_VOLUME_TO_DETACH_IN_SECONDS, 1);
            layout.setComment(MAX_WAIT_TIME_VOLUME_TO_DETACH_IN_SECONDS, "Max wait time for volume to be detached, in seconds");
            propertiesConfiguration.setProperty(MAX_WAIT_TIME_VOLUME_TO_DETACH_IN_SECONDS, Integer.toString(defaultMaxWaitTimeToDetachVolume));

            propertiesConfiguration.write(new FileWriter(file));
            LOG.debug("Config file {} stored successfully.", file.getPath());
        } catch (Exception ioException) {
            LOG.error("Can not create property file", ioException);
            throw new ConfigurationException("Can not create property file. Check path or permission: "
                    + file.getAbsolutePath(), ioException);
        }
    }

    public void createDBAndStoreSettings(final InitController.ConfigDto config) {
        if (!requiredTablesExist()) { // check if tables corrupted
            LOG.info("Initialization DB");
            dropDbTables();
            createDbAndStoreData(config);
        } else {
            storeAdminUserIfProvided();
            if (!isConfigurationStored()) {
                storeConfiguration(config);
            }
        }
    }

    private boolean isConfigurationStored() {
        Configuration loadedConf = mapper.load(Configuration.class, EC2MetadataUtils.getInstanceId());
        return loadedConf != null;
    }

    private void createDbAndStoreData(final InitController.ConfigDto config) {
        createDbStructure();
        storeAdminUserIfProvided();
        storeConfiguration(config);
    }

    private void createDbStructure() throws ConfigurationException {
        createTable("BackupList", 50L, 20L, "volumeId", "S", "fileName", "S");
        createTable("Configurations", 10L, 10L, "configurationId", "S");
        createTable("Retention", 50L, 20L, "volumeInstanceId", "S");
        createTable("Tasks", 50L, 20L, "id", "S");
        createTable("Snapshots", 50L, 20L, "volumeInstanceId", "S");
        createTable("Users", 50L, 20L, "id", "S");
    }

    private void createTable(String tableName, long readCapacityUnits, long writeCapacityUnits,
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
        if (userDto != null && adminPassword != null) {
            userDto.setEmail(userDto.getEmail().toLowerCase());
            User userToCreate = UserDtoConverter.convert(userDto);
            userToCreate.setPassword(DigestUtils.sha512Hex(adminPassword));
            userToCreate.setInstanceId(EC2MetadataUtils.getInstanceId());
            userToCreate.setRole("admin");
            mapper.save(userToCreate);
        }
    }

    private void storeConfiguration(final InitController.ConfigDto config) {
        Configuration configuration = convertToWorkerConfiguration(initConfigurationDto, config);
        mapper.save(configuration);
    }

    private Configuration convertToWorkerConfiguration(InitConfigurationDto dto, final InitController.ConfigDto config) {
        Configuration configuration = new Configuration();
        configuration.setConfigurationId(EC2MetadataUtils.getInstanceId());
        configuration.setEc2Region(Regions.getCurrentRegion().getName());
        configuration.setSdfsMountPoint(dto.getSdfs().getMountPoint());
        configuration.setSdfsVolumeName(dto.getSdfs().getVolumeName());
        configuration.setS3Bucket(config.getBucketName());
        configuration.setSdfsSize(config.getVolumeSize());

        // set default properties
        configuration.setRestoreVolumeIopsPerGb(restoreVolumeIopsPerGb);
        configuration.setRestoreVolumeType(restoreVolumeType);
        configuration.setTempVolumeIopsPerGb(tempVolumeIopsPerGb);
        configuration.setTempVolumeType(tempVolumeType);
        configuration.setSdfsLocalCacheSize(sdfsLocalCacheSize);
        configuration.setAmazonRetryCount(amazonRetryCount);
        configuration.setAmazonRetrySleep(amazonRetrySleep);
        configuration.setMaxQueueSize(queueSize);
        configuration.setSdfsConfigPath(sdfsConfigPath);
        configuration.setSdfsBackupFileName(sdfsStateBackupFileName);
        configuration.setRetentionCronExpression(defaultRetentionCronExpression);
        configuration.setWorkerDispatcherPollingRate(defaultPollingRate);
        configuration.setWaitTimeBeforeNewSyncWithAWS(defaultWaitTimeBeforeNewSyncWithAWS);
        configuration.setMaxWaitTimeToDetachVolume(defaultMaxWaitTimeToDetachVolume);
        return configuration;
    }

    public void syncSettingsInDbAndConfigFile() {
        // sync properties in DB with conf file
        boolean configurationChanged = false;
        Configuration loadedConf = mapper.load(Configuration.class, EC2MetadataUtils.getInstanceId());

        if (cronExpressionIsValid(retentionCronExpression) && !retentionCronExpression.equals(loadedConf.getRetentionCronExpression())) {
            LOG.debug("Applying new cron expression {} for Retention policy.", retentionCronExpression);
            loadedConf.setRetentionCronExpression(retentionCronExpression);
            configurationChanged = true;
        }
        if (propertyProvided(pollingRate) && pollingRate != loadedConf.getWorkerDispatcherPollingRate()) {
            LOG.debug("Applying new polling rate to pick up new task {} ms.", pollingRate);
            loadedConf.setWorkerDispatcherPollingRate(pollingRate);
            configurationChanged = true;
        }
        if (propertyProvided(waitTimeBeforeNewSync) && waitTimeBeforeNewSync != loadedConf.getWaitTimeBeforeNewSyncWithAWS()) {
            LOG.debug("Applying new wait time before new sync of Snapshot/Volume with AWS data {} seconds.", waitTimeBeforeNewSync);
            loadedConf.setWaitTimeBeforeNewSyncWithAWS(waitTimeBeforeNewSync);
            configurationChanged = true;
        }
        if (propertyProvided(maxWaitTimeToDetachVolume) && maxWaitTimeToDetachVolume != loadedConf.getMaxWaitTimeToDetachVolume()) {
            LOG.debug("Applying new max wait time for volume to be detach {} seconds.", maxWaitTimeToDetachVolume);
            loadedConf.setMaxWaitTimeToDetachVolume(maxWaitTimeToDetachVolume);
            configurationChanged = true;
        }
        if (configurationChanged) {
            LOG.debug("Storing updated settings from config file to DB.");
            mapper.save(loadedConf);
        }
    }

    @Override
    public void removeProperties() {
        File file = Paths.get(System.getProperty(catalinaHomeEnvPropName), confFolderName, propFileName).toFile();
        if (file.exists()) {
            file.delete();
        }
    }

    @Override
    public boolean areCredentialsValid() {
        AmazonEC2Client ec2Client = new AmazonEC2Client(credentialsProvider);
        ec2Client.setRegion(region);
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
        if (credentialsProvider.getCredentials() != null) {
            validateCredentials(credentialsProvider.getCredentials().getAWSAccessKeyId(), credentialsProvider.getCredentials().getAWSSecretKey());
            return true;
        } else {
            return false;
        }

    }

    @Override
    public boolean propertyFileExists() {
        return getPropertyFile().exists();
    }

    @Override
    public boolean checkDefaultUser(String login, String password) {
        return DEFAULT_LOGIN.equals(login.toLowerCase()) && password.equals(instanceId);
    }

    private List<InitConfigurationDto.S3> getBucketsWithSdfsMetadata() {
        ArrayList<InitConfigurationDto.S3> result = new ArrayList<>();

        try {
            AmazonS3Client client = new AmazonS3Client(credentialsProvider);
            List<Bucket> allBuckets = client.listBuckets();
            String bucketName = ENHANCED_SNAPSHOT_BUCKET_PREFIX + instanceId;
            result.add(new InitConfigurationDto.S3(bucketName, false));

            String currentLocation = region.toString();
            if (currentLocation.equalsIgnoreCase(Regions.US_EAST_1.getName())) {
                currentLocation = "US";
            }
            for (Bucket bucket : allBuckets) {
                try {
                    if (bucket.getName().startsWith(ENHANCED_SNAPSHOT_BUCKET_PREFIX)) {
                        String location = client.getBucketLocation(bucket.getName());

                        // Because client.getBucketLocation(bucket.getName()) returns US if bucket is in us-east-1
                        if (!location.equalsIgnoreCase(currentLocation) && !location.equalsIgnoreCase("US")) {
                            continue;
                        }

                        ListObjectsRequest request = new ListObjectsRequest()
                                .withBucketName(bucket.getName()).withPrefix(FilenameUtils.removeExtension(sdfsStateBackupFileName));
                        if (client.listObjects(request).getObjectSummaries().size() > 0) {
                            if (bucketName.equals(bucket.getName())) {
                                result.get(0).setCreated(true);
                            } else {
                                result.add(new InitConfigurationDto.S3(bucket.getName(), true));
                            }
                        }
                    }
                } catch (Exception ignored) {
                    // If any exception appears during working with bucket,
                    // just skip this bucket and try to scan the next one
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
        InitConfigurationDto.SDFS sdfs = new InitConfigurationDto.SDFS();
        sdfs.setMountPoint(mountPoint);
        sdfs.setVolumeName(volumeName);
        int maxVolumeSize = getMaxVolumeSize();
        sdfs.setMaxVolumeSize(String.valueOf(maxVolumeSize));
        sdfs.setVolumeSize(String.valueOf(Math.min(maxVolumeSize, Integer.parseInt(defaultVolumeSize))));
        sdfs.setMinVolumeSize(minVolumeSize);
        sdfs.setCreated(sdfsAlreadyExists());

        initConfigurationDto.setS3(getBucketsWithSdfsMetadata());
        initConfigurationDto.setSdfs(sdfs);
        return initConfigurationDto;
    }

    private boolean requiredTablesExist() {
        AmazonDynamoDBClient amazonDynamoDB = new AmazonDynamoDBClient(credentialsProvider);
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
        AmazonDynamoDBClient client = new AmazonDynamoDBClient(credentialsProvider);
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


    private boolean sdfsAlreadyExists() {
        LOG.info("sdfs already exists...");
        File configf = new File(sdfsConfigPath);
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
    }

    private File getPropertyFile() {
        return Paths.get(System.getProperty(catalinaHomeEnvPropName), confFolderName, propFileName).toFile();
    }

    @Override
    public String getInstanceId() {
        return instanceId;
    }


    @Override
    public void configureAWSLogAgent() {
        try {
            replaceInFile(new File(awscliConfPath), "<region>", region.toString());
            replaceInFile(new File(awslogsConfPath), "<instance-id>", instanceId);
        } catch (Exception e) {
            LOG.warn("Cant initialize AWS Log agent");
        }
    }

    @Override
    public void validateVolumeSize(final String volumeSize) {
        int size = Integer.parseInt(volumeSize);
        int min = Integer.parseInt(minVolumeSize);
        int max = getMaxVolumeSize();
        if (size < min || size > max) {
            throw new ConfigurationException("Invalid volume size");
        }
    }

    private void replaceInFile(File file, String marker, String value) throws IOException {
        String lines[] = FileUtils.readLines(file).toArray(new String[1]);
        for (int i = 0; i < lines.length; i++) {
            if (lines[i].contains(marker)) {
                lines[i] = lines[i].replace(marker, value);
            }
        }
        FileUtils.writeLines(file, Arrays.asList(lines));
    }

    public int getMaxVolumeSize() {
        UnixOperatingSystemMXBean osBean = (UnixOperatingSystemMXBean) ManagementFactory.getOperatingSystemMXBean();
        //Total RAM - RAM available for Tomcat - reserved
        long totalRAM = osBean.getFreePhysicalMemorySize() - Runtime.getRuntime().maxMemory() - SYSTEM_RESERVED_RAM_IN_BYTES - SDFS_RESERVED_RAM_IN_BYTES;
        int maxVolumeSize = (int) (totalRAM / BYTES_IN_GB) * SDFS_VOLUME_SIZE_IN_GB_PER_GB_OF_RAM;
        return maxVolumeSize;
    }

    private void dropDbTables() {
        DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
        ListTablesResult listResult = amazonDynamoDB.listTables();
        List<String> tableNames = listResult.getTableNames();
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

    // when applications starts for the first time property file in conf directory does not exist yet
    // in case user removes some properties from file this check will help to avoid unwanted exceptions
    private boolean propertyProvided(int property) {
        return property != propertyNotProvided;
    }

    private boolean cronExpressionIsValid(String cronExpression) {
        try {
            // Unix cron does not contain seconds but spring cron does
            new CronSequenceGenerator("0 " + cronExpression);
            return true;
        } catch (Exception e) {
            LOG.warn("Provided cron expression {} is invalid. Changes will not be applied", cronExpression);
            return false;
        }

    }

}

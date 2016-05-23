package com.sungardas.enhancedsnapshots.service.impl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.PostConstruct;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapperConfig;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBScanExpression;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.BackupEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.Configuration;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.RetentionEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.SnapshotEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.TaskEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.exception.EnhancedSnapshotsException;
import com.sungardas.enhancedsnapshots.service.SystemService;

import com.sungardas.enhancedsnapshots.service.upgrade.SystemUpgrade;
import org.codehaus.jackson.map.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * Implementation for {@link SystemService}
 */
@Service
public class SystemServiceImpl implements SystemService {
    private static final String CURRENT_VERSION = "0.0.1";
    private static final String LATEST_VERSION = "latest-version";
    private static final String INFO_URL = "http://com.sungardas.releases.s3.amazonaws.com/info";

    private static final String TEMP_DIRECTORY_PREFIX = "systemBackupFiles";
    private static final String INFO_FILE_NAME = "info";
    private static final String VERSION_KEY = "version";
    private static final String PROPERTY_FILE_PATH = "/opt/apache-tomcat-8.0.24/conf/enhancedsnapshots.properties";
    private static final String NGINX_CERTIFICATE_FILE_PATH = "/etc/nginx/cert.crt";
    private static final String NGINX_KEY_FILE_PATH = "/etc/nginx/cert.key";

    @Autowired
    private DynamoDBMapperConfig config;

    @Autowired
    private AmazonDynamoDB amazonDynamoDB;

    @Autowired
    private SystemUpgrade systemUpgrade;

    private final ObjectMapper objectMapper = new ObjectMapper();

    private DynamoDBMapper dynamoDBMapper;

    @PostConstruct
    private void init() {
        dynamoDBMapper = new DynamoDBMapper(amazonDynamoDB, config);
        backup();
    }


    @Override
    public void backup() {
        try {
            Path tempDirectory = Files.createTempDirectory(TEMP_DIRECTORY_PREFIX);
            addInfo(tempDirectory);
            storeFiles(tempDirectory);
            backupDB(tempDirectory);
        } catch (IOException e) {
            throw new EnhancedSnapshotsException(e);
        }


    }

    @Override
    public void restore(final String bucketName) {

    }


    private void addInfo(final Path tempDirectory) throws IOException {
        File dest = Paths.get(tempDirectory.toString(), INFO_FILE_NAME).toFile();
        Map<String, String> info = new HashMap<>();
        info.put(VERSION_KEY, CURRENT_VERSION);
        objectMapper.writeValue(dest, info);
    }

    private void backupDB(Path tempDirectory) throws IOException {
        storeTable(BackupEntry.class, tempDirectory);
        storeTable(Configuration.class, tempDirectory);
        storeTable(RetentionEntry.class, tempDirectory);
        storeTable(SnapshotEntry.class, tempDirectory);
        storeTable(TaskEntry.class, tempDirectory);
        storeTable(User.class, tempDirectory);
    }

    private void storeTable(Class tableClass, Path tempDirectory) throws IOException {
        File dest = Paths.get(tempDirectory.toString(), tableClass.getName()).toFile();
        List result = dynamoDBMapper.scan(tableClass, new DynamoDBScanExpression());
        objectMapper.writeValue(dest, result);
    }

    private void storeFiles(Path tempDirectory) throws IOException {
        //Property file
        Files.copy(Paths.get(PROPERTY_FILE_PATH), tempDirectory);
        //nginx certificates
        Files.copy(Paths.get(NGINX_CERTIFICATE_FILE_PATH), tempDirectory);
        Files.copy(Paths.get(NGINX_KEY_FILE_PATH), tempDirectory);
    }
}

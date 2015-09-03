package com.sungardas.snapdirector.service.impl;

import com.amazonaws.AmazonClientException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.ec2.AmazonEC2Client;
import com.sungardas.snapdirector.exception.ConfigurationException;
import com.sungardas.snapdirector.service.CredentialsService;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Service;

import javax.validation.constraints.NotNull;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Properties;

@Service
public class CredentialsServiceImpl implements CredentialsService {
    private final String catalinaHomeEnvPropName = "catalina.home";
    private final String confFolderName = "conf";
    private final String propFileName = "amazon.properties";
    private final String accessKeyPropName = "amazon.aws.accesskey";
    private final String secretKeyPropName = "amazon.aws.secretkey";
    private static final Log LOG = LogFactory.getLog(CredentialsServiceImpl.class);
    private AWSCredentials credentials = null;

    @Override
    public void setCredentials(@NotNull String accessKey, @NotNull String secretKey) {
        validateCredentials(accessKey,secretKey);

        Properties properties = new Properties();
        File file = Paths.get(System.getProperty(catalinaHomeEnvPropName), confFolderName, propFileName).toFile();
            try {
                properties.setProperty(accessKeyPropName, accessKey);
                properties.setProperty(secretKeyPropName, secretKey);

                properties.store(new FileOutputStream(file), "AWS Credentials");
            } catch (IOException ioException) {
                throw new ConfigurationException("Can not create amazon.properties file\n" +
                        "Check path or permission: " + file.getAbsolutePath(), ioException);
            }
    }

    private void validateCredentials(String accessKey, String secretKey) {
        if(accessKey==null || accessKey.length()==0) {
            throw new ConfigurationException("Null or empty AWS AccessKey");
        }
        if(secretKey==null || secretKey.length()==0) {
            throw new ConfigurationException("Null or empty AWS SecretKey");
        }
    }

    @Override
    public boolean areCredentialsValid() {
        getCredentials();
        AmazonEC2Client ec2Client = new AmazonEC2Client(getCredentials());
        try {
            ec2Client.describeRegions();
            return true;
        } catch (AmazonClientException e) {
            LOG.warn("Provided AWS credentials are invalid.");
            return false;
        } catch (ConfigurationException credentialsNotProvided) {
            LOG.error(credentialsNotProvided.getMessage(), credentialsNotProvided);
            LOG.error("Set AWS Credentials before use areCredentialsValid method");
            throw credentialsNotProvided;
        }

    }

    @Override
    public boolean isAwsPropertyFileExists() {
        return getPropertyFile().exists();
    }


    @Override
    public AWSCredentials getCredentials() {
        if(credentials == null) {
            refresh();
        }
        return credentials;
    }

    @Override
    public void refresh() {
        try {
            Properties properties = new Properties();
            File file = getPropertyFile();
            properties.load(new FileInputStream(file));
            credentials = new BasicAWSCredentials(properties.getProperty(accessKeyPropName),
                    properties.getProperty(secretKeyPropName));
        } catch (IOException ioException){
            ConfigurationException awsPropsNotExists = new ConfigurationException(propFileName+" file does not exists" , ioException);
            LOG.error(awsPropsNotExists);
            throw awsPropsNotExists;
        }
    }

    private File getPropertyFile() {
        return Paths.get(System.getProperty(catalinaHomeEnvPropName), confFolderName, propFileName).toFile();
    }
}

package com.sungardas.init;

import com.amazonaws.AmazonClientException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.ec2.AmazonEC2Client;
import com.sungardas.snapdirector.dto.InitConfigurationDto;
import com.sungardas.snapdirector.exception.ConfigurationException;
import com.sungardas.snapdirector.exception.SnapdirectorException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import javax.validation.constraints.NotNull;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.nio.file.Paths;
import java.util.Properties;
import java.util.Scanner;

@Service
public class CredentialsServiceImpl implements CredentialsService {
    private final String catalinaHomeEnvPropName = "catalina.home";
    private final String confFolderName = "conf";
    private final String propFileName = "amazon.properties";
    private final String accessKeyPropName = "amazon.aws.accesskey";
    private final String secretKeyPropName = "amazon.aws.secretkey";
    private static final Log LOG = LogFactory.getLog(CredentialsServiceImpl.class);
    private AWSCredentials credentials = null;
    private final String DEFAULT_LOGIN = "admin@snapdirector";
    private String instanceId;

    @PostConstruct
    private void init(){
        instanceId = getInstanceId();
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
            properties.setProperty(accessKeyPropName, credentials.getAWSAccessKeyId());
            properties.setProperty(secretKeyPropName, credentials.getAWSSecretKey());

            properties.store(new FileOutputStream(file), "AWS Credentials");
        } catch (IOException ioException) {
            throw new ConfigurationException("Can not create amazon.properties file\n" +
                    "Check path or permission: " + file.getAbsolutePath(), ioException);
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
        } catch (ConfigurationException credentialsNotProvided) {
            LOG.error(credentialsNotProvided.getMessage(), credentialsNotProvided);
            LOG.error("Set AWS Credentials before use areStoredCredentialsValid method");
            throw credentialsNotProvided;
        }
    }

    @Override
    public boolean isAwsPropertyFileExists() {
        return getPropertyFile().exists();
    }

    @Override
    public AWSCredentials getCredentials() {
        return credentials;
    }

    @Override
    public boolean checkDefaultUser(String login, String password) {
        return DEFAULT_LOGIN.equals(login.toLowerCase()) && password.equals(instanceId);
    }

    @Override
    public InitConfigurationDto getInitConfigurationDto() {
        InitConfigurationDto initConfigurationDto = new InitConfigurationDto();


        return initConfigurationDto;
    }

    private void validateCredentials(String accessKey, String secretKey) {
        if(accessKey==null || accessKey.isEmpty()) {
            throw new ConfigurationException("Null or empty AWS AccessKey");
        }
        if(secretKey==null || secretKey.isEmpty()) {
            throw new ConfigurationException("Null or empty AWS SecretKey");
        }
    }

    private File getPropertyFile() {
        return Paths.get(System.getProperty(catalinaHomeEnvPropName), confFolderName, propFileName).toFile();
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

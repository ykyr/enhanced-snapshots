package com.sungardas.snapdirector.aws;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;

import static java.lang.String.format;

public class PropertyBasedCredentialsProvider implements
		AWSCredentialsProvider {
	private static final Log LOG = LogFactory.getLog(PropertyBasedCredentialsProvider.class);
	private static BasicAWSCredentials credentials;

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
            File file = Paths.get(System.getProperty("catalina.home"), "conf", "amazon.properties").toFile();
            properties.load(new FileInputStream(file));
            credentials = new BasicAWSCredentials(properties.getProperty("amazon.aws.accesskey"),
                    properties.getProperty("amazon.aws.secretkey"));
        } catch (IOException e){
            LOG.error(e);
        }
	}

}

package com.sangardas.snapshotdirector.aws;

import java.io.IOException;
import java.io.InputStream;

import com.amazonaws.AmazonClientException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.auth.PropertiesCredentials;


public class PropertiesResourceFileCredentialsProvider implements AWSCredentialsProvider{
	private final String filename;
	
	public PropertiesResourceFileCredentialsProvider(String filename) {
		if (filename == null)
            throw new IllegalArgumentException(
                    "Credentials file path cannot be null");
		this.filename = filename;
	}

	@Override
	public AWSCredentials getCredentials() {
		InputStream is = Thread.currentThread().getContextClassLoader().getResourceAsStream(filename);
		
		try {
            return new PropertiesCredentials(is);
        } catch (IOException e) {
            throw new AmazonClientException(
                    "Unable to load AWS credentials from the "
                            + filename + " file", e);
        }
	}

	@Override
	public void refresh() {	
	}
	
	@Override
    public String toString() {
        return getClass().getSimpleName() + "(" + filename + ")";
    }

}

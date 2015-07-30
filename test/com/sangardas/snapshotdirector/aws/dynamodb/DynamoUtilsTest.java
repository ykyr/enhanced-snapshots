package com.sangardas.snapshotdirector.aws.dynamodb;

import static org.junit.Assert.*;

import java.util.List;

import org.junit.Test;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapperConfig;
import com.amazonaws.services.ec2.model.Region;
import com.sangardas.snapshotdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sangardas.snapshotdirector.aws.dynamodb.model.BackupEntry;

public class DynamoUtilsTest {

	
	private AmazonDynamoDBClient client = new AmazonDynamoDBClient(new EnvironmentBasedCredentialsProvider());
	private DynamoDBMapper mapper = new DynamoDBMapper(client);
	
	@Test
	public void testGetVolumeBackups() {
		
		
		List<BackupEntry> items = DynamoUtils.getBackupInfo("vol-69dee6a0", this.mapper);

		assertNotNull(items);
		assertFalse(items.isEmpty());
		
	}
	
	@Test
	public void testAuthenticateUser(){
		boolean info = DynamoUtils.authenticateUser("admin@sungard.com", "admin", mapper);
		assertNotNull(info);
		assertTrue(info);
		
	}
	
	@Test
	public void testGetFullUserInfo(){
		String info = DynamoUtils.getFullUserInfoByEmail("admin@sungard.com", mapper);
		assertNotNull(info);
		assertTrue(!info.isEmpty());
		
	}
}

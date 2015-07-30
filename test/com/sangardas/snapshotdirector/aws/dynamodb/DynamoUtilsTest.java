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

	@Test
	public void testGetVolumeBackups() {
		
		AmazonDynamoDBClient client = new AmazonDynamoDBClient(new EnvironmentBasedCredentialsProvider());
		DynamoDBMapper mapper = new DynamoDBMapper(client);
		List<BackupEntry> items = DynamoUtils.getBackupInfo("vol-69dee6a0", mapper);

		assertNotNull(items);
		assertFalse(items.isEmpty());
		
	}
}

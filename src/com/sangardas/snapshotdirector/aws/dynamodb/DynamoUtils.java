package com.sangardas.snapshotdirector.aws.dynamodb;

import java.util.List;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapperConfig;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBQueryExpression;
import com.amazonaws.services.ec2.model.Volume;
import com.sangardas.snapshotdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sangardas.snapshotdirector.aws.dynamodb.model.BackupEntry;
/**
 * DynamoUtils is a Repository for static methods which manipulate data in DynamoDB </br>
 * 
 * @author dvas
 *
 */
public class DynamoUtils {
	
	/**
	 * Returns the list of {@link BackupEntry} objects which represent records in DB.
	 * @param volumeId {@link String}
	 * @return {@link List} 
	 */
	public static List<BackupEntry> getBackupInfo(String volumeId, DynamoDBMapper mapper) {

		BackupEntry backupEntry = new BackupEntry();
		backupEntry.setVolumeId(volumeId);

		DynamoDBQueryExpression<BackupEntry> expression = new DynamoDBQueryExpression<BackupEntry>()
				.withHashKeyValues(backupEntry);

		List<BackupEntry> backupEntries = mapper.query(BackupEntry.class,
				expression);

		return backupEntries;

	}
	
	public static List<BackupEntry> getBackupInfo(Volume volume, DynamoDBMapper mapper){
		return getBackupInfo(volume.getVolumeId(), mapper);
	}
	
	public void putBackupInfo(List<BackupEntry> bakupEntries, DynamoDBMapper mapper){
		//TODO: Overload this method
		
		DynamoDBMapperConfig config = new DynamoDBMapperConfig(DynamoDBMapperConfig.SaveBehavior.CLOBBER);
		mapper.batchWrite(bakupEntries, null, config);
		
		
	}
	
	public void removeBackupInfo(){
		//TODO: Implement removeBackupInfo
	}

}

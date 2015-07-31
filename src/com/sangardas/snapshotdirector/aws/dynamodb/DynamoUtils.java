package com.sangardas.snapshotdirector.aws.dynamodb;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.codec.digest.DigestUtils;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapperConfig;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBQueryExpression;
import com.amazonaws.services.ec2.model.Volume;
import com.sangardas.snapshotdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sangardas.snapshotdirector.aws.dynamodb.model.BackupEntry;
import com.sangardas.snapshotdirector.aws.dynamodb.model.User;

/**
 * DynamoUtils is a Repository for static methods which manipulate data in
 * DynamoDB </br>
 * 
 * @author dvas
 *
 */
public class DynamoUtils {

	/**
	 * Returns the list of {@link BackupEntry} objects which represent records
	 * in DB.
	 * 
	 * @param volumeId
	 *            {@link String}
	 * @return {@link List}
	 */
	public static List<BackupEntry> getBackupInfo(String volumeId,
			DynamoDBMapper mapper) {

		BackupEntry backupEntry = new BackupEntry();
		backupEntry.setVolumeId(volumeId);

		DynamoDBQueryExpression<BackupEntry> expression = new DynamoDBQueryExpression<BackupEntry>()
				.withHashKeyValues(backupEntry);

		List<BackupEntry> backupEntries = mapper.query(BackupEntry.class,
				expression);

		return backupEntries;

	}

	public static List<BackupEntry> getBackupInfo(Volume volume,
			DynamoDBMapper mapper) {
		return getBackupInfo(volume.getVolumeId(), mapper);
	}

	public static void putBackupInfo(List<BackupEntry> bakupEntries,
			DynamoDBMapper mapper) {
		
		DynamoDBMapperConfig config = new DynamoDBMapperConfig(
				DynamoDBMapperConfig.SaveBehavior.CLOBBER);
		mapper.batchWrite(bakupEntries, new ArrayList<BackupEntry>(), config);

	}
	
	public static void putbackupInfo(BackupEntry entry, DynamoDBMapper mapper){
		List<BackupEntry> singleEntryList = new ArrayList<BackupEntry>();
		singleEntryList.add(entry);
		
		putBackupInfo(singleEntryList, mapper);
		
	}

	public static void removeBackupInfo() {
		// TODO: Implement removeBackupInfo
	}

	public static boolean authenticateUser(String email, String pass,
			DynamoDBMapper mapper) {
		User user = getUser(email, mapper);
		if (user.getPassword().equals(getPasswordHash(pass))) {
			return true;
		}

		return false;

	}

	public static String getFullUserInfoByEmail(String email, DynamoDBMapper mapper) {
		return getUser(email, mapper).getUserInfo();
	}

	private static User getUser(String email, DynamoDBMapper mapper) {
		User user = new User();
		user.setEmail(email);
		DynamoDBQueryExpression<User> expression = new DynamoDBQueryExpression<User>()
				.withHashKeyValues(user)
				.withConsistentRead(false);

		List<User> userEntries = mapper.query(User.class, expression);

		if (userEntries == null || userEntries.isEmpty()) {
			return null;
		}

		return userEntries.get(0);
	}

	private static String getPasswordHash(String pass) {
		return DigestUtils.sha512Hex(pass);
	}

}

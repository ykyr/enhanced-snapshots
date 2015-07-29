package com.sangardas.snapshotdirector.aws.dynamodb;

import static org.junit.Assert.*;

import java.util.List;

import org.junit.Test;

import com.sangardas.snapshotdirector.aws.dynamodb.model.BackupEntry;

public class DynamoUtilsTest {

	@Test
	public void testGetVolumeBackups() {
		List<BackupEntry> items = DynamoUtils.getBackupInfo("vol-69dee6a0");
		
		assertNotNull(items);
		assertFalse(items.isEmpty());
		
		
		for (BackupEntry backupList : items) {
			System.out.println(backupList);
		}
		
		
	}

}

package com.sungardas.snapdirector.aws.dynamodb;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sungardas.snapdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupState;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class DynamoUtilsTest {


    private AmazonDynamoDBClient client = new AmazonDynamoDBClient(new EnvironmentBasedCredentialsProvider());
    private DynamoDBMapper mapper = new DynamoDBMapper(client);
    
    @Test
    public void testPutBackupInfo() {

        double salt = Math.random();

        BackupEntry newBackup = new BackupEntry("vol-69dee6a0" + salt, "vol-69dee6a0111.backup", "201507311025", "111111", BackupState.INPROGRESS, "id", null, null, null, null);


        List<BackupEntry> items = new ArrayList<BackupEntry>();
        items.add(newBackup);

        DynamoUtils.putBackupInfo(items, mapper);

        BackupEntry fetched = DynamoUtils.getBackupInfo("vol-69dee6a0" + salt, mapper).get(0);

        assertNotNull(fetched);
        assertTrue(newBackup.equals(fetched));

    }

}

package com.sungardas.snapdirector.tasks;

import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sungardas.snapdirector.aws.dynamodb.DynamoUtils;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupState;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.tasks.Task;

public class BackupFakeTask implements Task {
	private static final Log LOG = LogFactory.getLog(AWSBackupVolumeTask.class);
	private AWSCredentialsProvider credentialsProvider;
	private TaskEntry taskEntry;
	public BackupFakeTask(AWSCredentialsProvider credentialsProvider, TaskEntry taskEntry) {
		this.credentialsProvider = credentialsProvider;
		this.taskEntry = taskEntry;
	}

	@Override
	public void execute() {
		LOG.info("Task "+taskEntry.getId()+": Change task state to 'inprogress'");
		DynamoUtils.deleteTask(taskEntry.getId(), getMapper());
		taskEntry.setStatus("running");
		DynamoUtils.putTask(taskEntry, getMapper());
		
		String timestamp = Long.toString(System.currentTimeMillis());
		String volumeId = taskEntry.getVolume();
		String filename = volumeId + "." + timestamp + ".backup";
		BackupEntry backup = new BackupEntry(taskEntry.getVolume(), filename, timestamp, "123456789", BackupState.COMPLETED);
		LOG.info("Task "+taskEntry.getId()+":put backup info'");
		DynamoUtils.putbackupInfo(backup, getMapper());
		
		try {
			TimeUnit.SECONDS.sleep(10);
		}catch(InterruptedException ignored){}
		
		DynamoUtils.deleteTask(taskEntry.getId(), getMapper());
		LOG.info("Task "+taskEntry.getId()+": Delete completed task:" + taskEntry.getId() );
		LOG.info("Task completed.");
	}
	
	private DynamoDBMapper getMapper() {
		AmazonDynamoDBClient client = new AmazonDynamoDBClient(credentialsProvider);
		client.setRegion(Region.getRegion(Regions.fromName("us-east-1")));
		return new DynamoDBMapper(client);
	}

}

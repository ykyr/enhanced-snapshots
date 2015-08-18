package com.sungardas.snapdirector.tasks;

import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sungardas.snapdirector.aws.dynamodb.DynamoUtils;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupState;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.TaskRepository;

@Component
@Scope("prototype")
public class BackupFakeTask implements Task {
	private static final Logger LOG = LogManager.getLogger(BackupFakeTask.class);
    
    @Autowired
	private TaskRepository taskRepository;
    
    @Autowired
    private AWSCredentials amazonAWSCredentials;
    
    private TaskEntry taskEntry;

    
    public void setTaskEntry(TaskEntry taskEntry) {
    	this.taskEntry= taskEntry;
    }

    @Override
    public void execute() {
        LOG.info("Task " + taskEntry.getId() + ": Change task state to 'inprogress'");
        taskEntry.setStatus("running");
        taskRepository.save(taskEntry);

        LOG.info(taskEntry.toString());
        String timestamp = Long.toString(System.currentTimeMillis());
        String volumeId = taskEntry.getVolume();
        String filename = volumeId + "." + timestamp + ".backup";
        BackupEntry backup = new BackupEntry(taskEntry.getVolume(), filename, timestamp, "123456789", BackupState.COMPLETED, taskEntry.getInstanceId());
        LOG.info("Task " + taskEntry.getId() + ":put backup info'");
        DynamoUtils.putbackupInfo(backup, getMapper());

        try {
            TimeUnit.SECONDS.sleep(10);
        } catch (InterruptedException ignored) {
        }

        DynamoUtils.deleteTask(taskEntry.getId(), getMapper());
        LOG.info("Task " + taskEntry.getId() + ": Delete completed task:" + taskEntry.getId());
        LOG.info("Task completed.");
    }

    private DynamoDBMapper getMapper() {
        AmazonDynamoDBClient client = new AmazonDynamoDBClient(amazonAWSCredentials);
        client.setRegion(Region.getRegion(Regions.fromName("us-east-1")));
        return new DynamoDBMapper(client);
    }

}

package com.sungardas.snapdirector.worker;

import static java.lang.String.format;

import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletContext;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.json.JSONObject;

import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.DeleteMessageRequest;
import com.amazonaws.services.sqs.model.Message;
import com.amazonaws.services.sqs.model.ReceiveMessageRequest;
import com.amazonaws.services.sqs.model.ReceiveMessageResult;
import com.sungardas.snapdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.tasks.AWSBackupVolumeTask;
import com.sungardas.snapdirector.tasks.BackupFakeTask;
import com.sungardas.snapdirector.tasks.Task;

public class TaskWorker implements Runnable {
	private  static final Log LOG = LogFactory.getLog(TaskWorker.class);
	private AWSCredentialsProvider credentialsProvider;
	private ServletContext context;
	
	public TaskWorker(ServletContext context) {
		this.context = context;
		credentialsProvider = new EnvironmentBasedCredentialsProvider();
	}
	
	@Override
	public void run() {
		
		DynamoDB dynamoDB = new DynamoDB(new AmazonDynamoDBClient(credentialsProvider));
		Table workerConfigurationTable = dynamoDB.getTable("WorkerConfiguration");
		
		Item configuration = workerConfigurationTable.getItem("workerId", context.getInitParameter("aws:worker-configuration"));
		LOG.info("Loaded Worker Configuration: " + configuration.toJSON()+"\n");
		String queueURL = context.getInitParameter("aws:sqs-queue-url");
		String region = context.getInitParameter("aws:sqs-region");
		String routineInstId = context.getInitParameter("aws:routine-inst-id");
		
		LOG.info("Starting tasks reciever...");
		LOG.info("Queue URL" + queueURL);
		LOG.info("Queue Region" + region);
		LOG.info("Queue Listener attribute:" + routineInstId + "\n\n");
		
		AmazonSQS sqs = newAmazonSQSClient(region);
		LOG.info(format("Starting listening to tasks queue: %s", queueURL));
		
		while (true) {
			sleep();
			LOG.info("\n\nLook for sended tasks..");
			ReceiveMessageRequest receiveMessageRequest = new ReceiveMessageRequest(queueURL);
			ReceiveMessageResult result = sqs.receiveMessage(receiveMessageRequest.withMessageAttributeNames("listener-"+routineInstId));
			List<Message> messages = result.getMessages();
			if(messages.size()>0) {
				String body = messages.get(0).getBody();
				LOG.info(format("Got message : %s", messages.get(0).getMessageId()));
				String messageRecieptHandle = messages.get(0).getReceiptHandle();
				sqs.deleteMessage(new DeleteMessageRequest(queueURL, messageRecieptHandle));
				
				Task task = null;
				TaskEntry entry = new TaskEntry(new JSONObject(body));
				switch (entry.getType()) {
				case "backup" :
					LOG.info("Task was identified as backup");
					task = new BackupFakeTask(credentialsProvider, entry);
					break;
				case "restore" :
				case "deleteBackupfile" :
				default:
					
					LOG.info("Task type not implemented");
				}
				
				if (task!=null) {
					task.execute();
				}
				
			}
			
		}
	}

	private void sleep() {
		try {
			TimeUnit.SECONDS.sleep(20);
		} catch (InterruptedException e) {	e.printStackTrace(); }
	}
	
	private DynamoDBMapper getMapper(ServletContext context) {
		AmazonDynamoDBClient client = new AmazonDynamoDBClient(new EnvironmentBasedCredentialsProvider());
		String region = context.getInitParameter("aws:dynamodb-region");
		client.setRegion(Region.getRegion(Regions.fromName(region)));
		return new DynamoDBMapper(client);
	}
	
	private AmazonSQS newAmazonSQSClient(String region) {
		AmazonSQS sqs = new AmazonSQSClient(credentialsProvider);
		Region sqsRegion = Region.getRegion(Regions.fromName(region));
		sqs.setRegion(sqsRegion);
		return sqs;
	}
}

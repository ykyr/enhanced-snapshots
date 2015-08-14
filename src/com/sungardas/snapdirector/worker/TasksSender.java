package com.sungardas.snapdirector.worker;

import static java.lang.String.format;

import java.util.HashMap;
import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletContext;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.MessageAttributeValue;
import com.amazonaws.services.sqs.model.SendMessageRequest;
import com.sungardas.snapdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sungardas.snapdirector.aws.dynamodb.DynamoUtils;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;

public class TasksSender implements Runnable {
	private  static final Log LOG = LogFactory.getLog(TasksSender.class);
	private ServletContext context;
	private AWSCredentialsProvider credentialsProvider;
	public TasksSender(ServletContext context) {
		this.context = context;
		credentialsProvider = new EnvironmentBasedCredentialsProvider();
	}
	
	public void run() {
		String queueURL = context.getInitParameter("aws:sqs-queue-url");
		String region = context.getInitParameter("aws:sqs-region");
		String routineInstId = context.getInitParameter("aws:routine-inst-id");
		
		LOG.info("Starting tasks reciever...");
		LOG.info("Queue URL" + queueURL);
		LOG.info("Queue Region" + region);
		LOG.info("Queue Listener attribute:" + routineInstId + "\n\n");
		
		AmazonSQS sqs = newAmazonSQSClient(region);
		LOG.info(format("Starting recieving to tasks queue: %s", queueURL));
		
		
		HashMap<String , MessageAttributeValue> messageAttributes = new HashMap<String, MessageAttributeValue>();
		messageAttributes.put("listener-"+routineInstId, new MessageAttributeValue().withDataType("String").withStringValue(routineInstId));
		
		
		while (true) {
			sleep();
			LOG.info("\n\nLook for waiting tasks..");
			List<TaskEntry> taskModels = DynamoUtils.getTasks(getMapper(context));
			for (TaskEntry entry : taskModels) {
				if (entry.getStatus().endsWith("waiting") && entry.getWorker().equals(routineInstId)) {
					DynamoUtils.deleteTask(entry.getId(), getMapper(context));
					entry.setStatus("sended");
					DynamoUtils.putTask(entry, getMapper(context));
					
					SendMessageRequest sendRequest = new SendMessageRequest(queueURL, entry.toString());
					sendRequest.setMessageAttributes(messageAttributes);
					sendRequest.setDelaySeconds(0);
					sqs.sendMessage(sendRequest);
					LOG.info("Sended message: \n" + entry.toString());
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
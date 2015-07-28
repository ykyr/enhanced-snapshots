package com.sangardas.snapshotdirector.worker;

import java.util.List;
import java.util.Map.Entry;
import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.DeleteMessageRequest;
import com.amazonaws.services.sqs.model.Message;
import com.amazonaws.services.sqs.model.ReceiveMessageRequest;
import com.amazonaws.util.json.JSONException;
import com.amazonaws.util.json.JSONObject;
import com.sangardas.snapshotdirector.tasks.AWSBackupVolumeTask;
import com.sangardas.snapshotdirector.tasks.Task;

import static java.lang.String.format;


public class AWSTaskWorker implements Runnable {
	public static final Log LOG = LogFactory.getLog(AWSTaskWorker.class);

	private AWSCredentialsProvider awsCredentialsProvider;
	private String queueURL;
	private String routineInstanceId;
	private String propertyFile;


	public AWSTaskWorker(AWSCredentialsProvider awsCredentialsProvider, String queueURL, String routineInstanceId,String propertyFile) {
		this.awsCredentialsProvider = awsCredentialsProvider;
		this.queueURL = queueURL;
		this.routineInstanceId = routineInstanceId;
		this.propertyFile = propertyFile;
	}


	@Override
	public void run() {

		AmazonSQS sqs = new AmazonSQSClient(awsCredentialsProvider);
		Region usWest2 = Region.getRegion(Regions.US_EAST_1);
		sqs.setRegion(usWest2);

		LOG.info(format("AWSTaskWorker: Starting listening queue %s", queueURL));
		try {
			while (true) {
				try {
					TimeUnit.SECONDS.sleep(2);
				} catch (InterruptedException e) {	e.printStackTrace(); }
				Task task = null;
				ReceiveMessageRequest receiveMessageRequest = new ReceiveMessageRequest(queueURL);
				List<Message> messages = sqs.receiveMessage(receiveMessageRequest).getMessages();
				if(messages!=null && messages.size()>0)
					LOG.info(format("AWSTaskWorker: resieved %d messages from: %s",messages.size(),queueURL));
				for (Message message : messages) {
					try {
						JSONObject taskMessage;
						taskMessage = new JSONObject(message.getBody());
						LOG.info(format("AWSTaskWorker: message resived: %s : %s",message.getMessageId(), taskMessage.toString()));
						String volumeId = taskMessage.getString("volume");
						boolean execNow = taskMessage.getJSONObject("scheduler").getBoolean("manual");
						
						task = new AWSBackupVolumeTask(awsCredentialsProvider,volumeId,routineInstanceId, propertyFile);
					} catch (JSONException e) {
						
						e.printStackTrace();
					}

					if(task!=null) {
						task.execute();
					}

					// Delete a message
					LOG.info(format("AWSTaskWorker: message deleted: %s", message.getMessageId()));
					String messageRecieptHandle = message.getReceiptHandle();
					sqs.deleteMessage(new DeleteMessageRequest(queueURL, messageRecieptHandle));
					
				
				}

			}

		} catch (AmazonServiceException ase) {
			System.out.println("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			System.out.println("Error Message:    " + ase.getMessage());
			System.out.println("HTTP Status Code: " + ase.getStatusCode());
			System.out.println("AWS Error Code:   " + ase.getErrorCode());
			System.out.println("Error Type:       " + ase.getErrorType());
			System.out.println("Request ID:       " + ase.getRequestId());
		} catch (AmazonClientException ace) {
			System.out.println("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, such as not "
					+ "being able to access the network.");
			System.out.println("Error Message: " + ace.getMessage());
		}
	}

}

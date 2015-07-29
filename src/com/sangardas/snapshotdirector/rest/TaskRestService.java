package com.sangardas.snapshotdirector.rest;

import static java.lang.String.format;

import javax.servlet.ServletContext;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.json.JSONArray;
import org.json.JSONObject;

import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.SendMessageRequest;
import com.amazonaws.services.sqs.model.SendMessageResult;
import com.sangardas.snapshotdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sangardas.snapshotdirector.rest.utils.JsonFromFile;


@Path("/task")
public class TaskRestService {
	public static final Log LOG = LogFactory.getLog(TaskRestService.class);

	@Context
	ServletContext context;


	@GET()
	@Produces(MediaType.APPLICATION_JSON)
	public String getTasks() {
		String result = null;
		try {
			//addTask(null);
			String path = context.getInitParameter("rest:mock-directory");
			JSONArray tasks = JsonFromFile.newJSONArray(path + "tasks.json");
			result = tasks.toString();
		} catch (Exception e) {
			throw new WebApplicationException(e);
		}
		return result;
	}


	@POST()
	@Produces(MediaType.APPLICATION_JSON)
	public String addTask(String task) {
		LOG.info("put message:" + task);
		String credentials = context.getInitParameter("aws:credentials-file");
		String sqsRegion = context.getInitParameter("aws:sqs-region");
		String queueURL = context.getInitParameter("aws:sqs-queue-url");
		AmazonSQS sqs = new AmazonSQSClient(new EnvironmentBasedCredentialsProvider());
		Region usWest2 = Region.getRegion(Regions.fromName(sqsRegion));
        sqs.setRegion(usWest2);
		
        String path = context.getInitParameter("rest:mock-directory");
        JSONObject newTask = JsonFromFile.newJSONObject(path + "newtask.json");
        //String body = newTask.toString();
        String body = task;
        try {
        SendMessageRequest sendRequest = new SendMessageRequest(queueURL, body);
        sendRequest.setDelaySeconds(0);
        SendMessageResult sendResult = sqs.sendMessage(sendRequest);
        LOG.info(format("TaskRestService: sended message: %s; body:%s", sendResult.getMessageId(),body)  );
        } catch (AmazonServiceException ase) {
            System.out.println("Caught an AmazonServiceException, which means your request made it " +
                    "to Amazon SQS, but was rejected with an error response for some reason.");
            System.out.println("Error Message:    " + ase.getMessage());
            System.out.println("HTTP Status Code: " + ase.getStatusCode());
            System.out.println("AWS Error Code:   " + ase.getErrorCode());
            System.out.println("Error Type:       " + ase.getErrorType());
            System.out.println("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
            System.out.println("Caught an AmazonClientException, which means the client encountered " +
                    "a serious internal problem while trying to communicate with SQS, such as not " +
                    "being able to access the network.");
            System.out.println("Error Message: " + ace.getMessage());
        }
		return null;
	}
}

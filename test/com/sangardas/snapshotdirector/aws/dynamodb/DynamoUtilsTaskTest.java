package com.sangardas.snapshotdirector.aws.dynamodb;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.json.JSONObject;
import org.junit.Test;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sungardas.snapdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sungardas.snapdirector.aws.dynamodb.DynamoUtils;
import com.sungardas.snapdirector.aws.dynamodb.model.Task;


public class DynamoUtilsTaskTest {

	private AmazonDynamoDBClient client = new AmazonDynamoDBClient(
			new EnvironmentBasedCredentialsProvider());
	private DynamoDBMapper mapper = new DynamoDBMapper(client);
	
	private Task taskToGet;
	private Task taskToDelete;

	@Test
	public void testPutTaskTaskDynamoDBMapper() {
		Task task = new Task("1", "running", "backup", "vol-00000000", "true",
				"admin", "2015-07-17 19:50:00");
		
		
		
		String retId = DynamoUtils.putTask(task, mapper);
		
		task.setId(retId);
		this.taskToGet = task;
		
		assertFalse(retId.isEmpty());
	}

	@Test
	public void testPutTaskJSONObjectDynamoDBMapper() {
		
		Map<String, Object> mapTask = new HashMap<String, Object>();
		mapTask.put("priority", "0");
		mapTask.put("status", "running");
		mapTask.put("type", "backup");
		mapTask.put("volume", "vol-00000001");
		mapTask.put("schedulerManual", "true");
		mapTask.put("schedulerName", "admin");
		mapTask.put("schedulerTime", "2015-07-17 19:50:00");
		
		JSONObject jsonTask = new JSONObject(mapTask);
		
		Task task = new Task(jsonTask);
		
		this.taskToDelete = task;

		String retId = DynamoUtils.putTask(task, mapper);
		
		task.setId(retId);
		this.taskToDelete = task;

		assertFalse(retId.isEmpty());
	}

	@Test
	public void testGetTask() {
		String gotTaskId = DynamoUtils.getTask(taskToGet.getId(), mapper);
		System.out.println(gotTaskId);
		assertFalse(gotTaskId.isEmpty());
		assertTrue(gotTaskId.equals(taskToGet.getId()));
	}

	@Test
	public void testDeleteTask() {
		DynamoUtils.deleteTask(taskToDelete.getId(), mapper);
		String deletedTaskId = DynamoUtils.getTask(taskToDelete.getId(), mapper);
		assertNull(deletedTaskId);
	}

}

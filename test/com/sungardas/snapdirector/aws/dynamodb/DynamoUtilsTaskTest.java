package com.sungardas.snapdirector.aws.dynamodb;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sungardas.snapdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;


public class DynamoUtilsTaskTest {

    private AmazonDynamoDBClient client = new AmazonDynamoDBClient(
            new EnvironmentBasedCredentialsProvider());
    private DynamoDBMapper mapper = new DynamoDBMapper(client);

    private TaskEntry taskToGet;
    private TaskEntry taskToDelete;

    @Before
    public void setup() {
        TaskEntry task = new TaskEntry("1", "running", "backup", "vol-00000000", "true",
                "admin", "2015-07-17 19:50:00", "id");

        String retId = DynamoUtils.putTask(task, mapper);

        task.setId(retId);
        this.taskToGet = task;

        Map<String, Object> mapTask = new HashMap<String, Object>();
        mapTask.put("id", "");
        mapTask.put("priority", "0");
        mapTask.put("status", "running");
        mapTask.put("type", "backup");
        mapTask.put("volume", "vol-00000001");
        mapTask.put("schedulerManual", "true");
        mapTask.put("schedulerName", "admin");
        mapTask.put("schedulerTime", "2015-07-17 19:50:00");
        mapTask.put("worker", "");
        mapTask.put("instanceId", "");

        JSONObject jsonTask = new JSONObject(mapTask);

        this.taskToDelete = new TaskEntry(jsonTask);
        taskToDelete.setId(DynamoUtils.putTask(task, mapper));
    }

    @Test
    public void testPutTaskTaskDynamoDBMapper() {
        assertFalse(taskToGet.getId().isEmpty());
    }

    @Test
    public void testPutTaskJSONObjectDynamoDBMapper() {
        assertFalse(taskToDelete.getId().isEmpty());
    }

    @Test
    public void testGetTask() {
        String gotTaskId = new JSONObject(DynamoUtils.getTask(taskToGet.getId(), mapper)).getString("id");
        System.out.println(gotTaskId);
        assertFalse(gotTaskId.isEmpty());
        assertTrue(gotTaskId.equals(taskToGet.getId()));
    }

    @Test
    public void testGetTasks() {
        List<TaskEntry> allTasks = DynamoUtils.getTasks(mapper);
        for (TaskEntry task : allTasks) {
            System.out.println(task);
        }
        assertFalse(allTasks.isEmpty());

    }

    @Test
    public void testDeleteTask() {
        DynamoUtils.deleteTask(taskToDelete.getId(), mapper);
        String deletedTaskId = DynamoUtils.getTask(taskToDelete.getId(), mapper);
        assertNull(deletedTaskId);
    }

}

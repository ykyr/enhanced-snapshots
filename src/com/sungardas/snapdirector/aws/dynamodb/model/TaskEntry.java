package com.sungardas.snapdirector.aws.dynamodb.model;

import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBAttribute;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBHashKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBTable;
import com.amazonaws.util.json.Jackson;
import org.json.JSONObject;

import java.util.LinkedHashMap;
import java.util.Map;


@DynamoDBTable(tableName = "Tasks")
public class TaskEntry {

    private final Map<String, Object> attributes = new LinkedHashMap<String, Object>();
    //private Scheduler scheduler;

    public TaskEntry() {
        super();
    }

    public TaskEntry(String priority, String status, String type, String volume,
                     String schedulerManual, String schedulerName, String schedulerTime, String instanceId) {
        this();

        this.attributes.put("priority", Integer.parseInt(priority));
        this.attributes.put("status", status);
        this.attributes.put("type", type);
        this.attributes.put("volume", volume);
        this.attributes.put("schedulerManual", schedulerManual);
        this.attributes.put("schedulerName", schedulerName);
        this.attributes.put("schedulerTime", schedulerTime);
        setInstanceId(instanceId);
    }

    public TaskEntry(JSONObject jsonTask) {
        this();

        try {
            this.setPriority(jsonTask.getInt("priority"));
        } catch (RuntimeException emptyPriority) {
            this.setPriority(0);
        }
        this.setStatus(jsonTask.getString("status"));
        this.setType(jsonTask.getString("type"));
        this.setVolume(jsonTask.getString("volume"));
        this.setSchedulerManual(jsonTask.getBoolean("schedulerManual"));
        this.setSchedulerName(jsonTask.getString("schedulerName"));
        this.setSchedulerTime(jsonTask.getString("schedulerTime"));
        this.setId(jsonTask.getString("id"));
        this.setWorker(jsonTask.getString("worker"));
        setInstanceId(jsonTask.getString("instanceId"));
    }


    @DynamoDBHashKey(attributeName = "id")
    public String getId() {
        return (String) attributes.get("id");
    }

    public void setId(String id) {
        attributes.put("id", id);
    }

    @DynamoDBAttribute(attributeName = "priority")
    public Integer getPriority() {
        return (Integer) attributes.get("priority");
    }

    public void setPriority(Integer priority) {
        attributes.put("priority", priority);
    }

    @DynamoDBAttribute(attributeName = "worker")
    public String getWorker() {
        return (String) attributes.get("worker");
    }

    public void setWorker(String worker) {
        attributes.put("worker", worker);
    }

    @DynamoDBAttribute(attributeName = "status")
    public String getStatus() {
        return (String) attributes.get("status");
    }

    public void setStatus(String status) {
        attributes.put("status", status);
    }

    @DynamoDBAttribute(attributeName = "type")
    public String getType() {
        return (String) attributes.get("type");
    }

    public void setType(String type) {
        attributes.put("type", type);
    }

    @DynamoDBAttribute(attributeName = "volume")
    public String getVolume() {
        return (String) attributes.get("volume");
    }

    public void setVolume(String volume) {
        attributes.put("volume", volume);
    }

    @DynamoDBAttribute(attributeName = "schedulerManual")
    public String getSchedulerManual() {
        return (String) attributes.get("schedulerManual");
    }

    public void setSchedulerManual(String schedulerManual) {
        attributes.put("schedulerManual", schedulerManual);
    }

    public void setSchedulerManual(Boolean isManual) {
        attributes.put("schedulerManual", isManual.toString());
    }

    @DynamoDBAttribute(attributeName = "schedulerName")
    public String getSchedulerName() {
        return (String) attributes.get("schedulerName");
    }

    public void setSchedulerName(String schedulerName) {
        attributes.put("schedulerName", schedulerName);
    }

    @DynamoDBAttribute(attributeName = "schedulerTime")
    public String getSchedulerTime() {
        return (String) attributes.get("schedulerTime");
    }

    public void setSchedulerTime(String schedulerTime) {
        attributes.put("schedulerTime", schedulerTime);
    }


    @DynamoDBAttribute(attributeName = "instanceId")
    public String getInstanceId() {
        return (String) attributes.get("instanceId");
    }

    public void setInstanceId(String instanceId) {
        attributes.put("instanceId", instanceId);
    }


    @Override
    public String toString() {
        return Jackson.toJsonString(attributes);
    }

}

package com.sangardas.snapshotdirector.aws.dynamodb.model;

import java.util.LinkedHashMap;
import java.util.Map;

import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBAttribute;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBHashKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBRangeKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBTable;
import com.amazonaws.util.json.Jackson;

@DynamoDBTable(tableName="BackupList")
final public class BackupEntry {
	private final Map<String, Object> attributes = new LinkedHashMap<String, Object>();
	
	@DynamoDBHashKey(attributeName="volumeId")
	public String getVolumeId() {
		return (String) attributes.get("volumeId");
	}
	public void setVolumeId(String volumeId) {
		attributes.put("volumeId", volumeId);
		
	}
	
	@DynamoDBRangeKey(attributeName="fileName")
	public String getFileName() {
		return (String) attributes.get("fileName");
	}
	public void setFileName(String fileName) {
		attributes.put("fileName", fileName);
	}
	
	@DynamoDBAttribute(attributeName="message")
	public String getMessage() {
		return (String) attributes.get("message");
	}
	public void setMessage(String message) {
		attributes.put("message", message);
	}
	
	@DynamoDBAttribute(attributeName="timeCreated")
	public String getTimeCreated() {
		return (String) attributes.get("timeCreated");
	}
	public void setTimeCreated(String timeCreated) {
		attributes.put("timeCreated", timeCreated);
	}
	
	@Override
	public String toString() {
		return Jackson.toJsonString(attributes);
	}
}

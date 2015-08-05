package com.sungardas.snapdirector.aws.dynamodb.model;

import java.util.LinkedHashMap;
import java.util.Map;

import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBAttribute;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBHashKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBRangeKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBTable;
import com.amazonaws.util.json.Jackson;

@DynamoDBTable(tableName = "BackupList")
final public class BackupEntry {
	private final Map<String, Object> attributes = new LinkedHashMap<String, Object>();

	
	public BackupEntry() {
		super();
	}
	
	public BackupEntry(String volumeId, String fileName, String message, String timeCreated){
		this.setVolumeId(volumeId);
		this.setFileName(fileName);
		this.setMessage(message);
		this.setTimeCreated(timeCreated);
	}
	
	
	@DynamoDBHashKey(attributeName = "volumeId")
	public String getVolumeId() {
		return (String) attributes.get("volumeId");
	}

	public void setVolumeId(String volumeId) {
		attributes.put("volumeId", volumeId);

	}

	@DynamoDBRangeKey(attributeName = "fileName")
	public String getFileName() {
		return (String) attributes.get("fileName");
	}

	public void setFileName(String fileName) {
		attributes.put("fileName", fileName);
	}

	@DynamoDBAttribute(attributeName = "message")
	public String getMessage() {
		return (String) attributes.get("message");
	}

	public void setMessage(String message) {
		attributes.put("message", message);
	}

	@DynamoDBAttribute(attributeName = "timeCreated")
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

	@Override
	public boolean equals(Object obj) {

		if (obj != null && obj instanceof BackupEntry) {
			if (this.getFileName().equals(((BackupEntry) obj).getFileName())
					&& this.getMessage().equals(
							((BackupEntry) obj).getMessage())
					&& this.getTimeCreated().equals(
							((BackupEntry) obj).getTimeCreated())
					&& this.getVolumeId().equals(
							((BackupEntry) obj).getVolumeId()))
				return true;
		}

		return false;
	}
}

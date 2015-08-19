package com.sungardas.snapdirector.aws.dynamodb.model;

import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBAttribute;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBHashKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBRangeKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBTable;
import com.amazonaws.util.json.Jackson;

import java.util.LinkedHashMap;
import java.util.Map;

@DynamoDBTable(tableName = "BackupList")
final public class BackupEntry {
	private final Map<String, Object> attributes = new LinkedHashMap<String, Object>();

	public BackupEntry() {
		super();
	}

	public BackupEntry(String volumeId, String fileName, String timeCreated, String backupSize, BackupState state,
			String instanceId, String snapshotId, String volumeType,String iops, String sizeGiB) {
		setVolumeId(volumeId);
		setFileName(fileName);
		setTimeCreated(timeCreated);
		setSize(backupSize);
		setState(state.getState());
		setInstanceId(instanceId);
		setSnapsotId(snapshotId);
		setVolumeType(volumeType);
		setIops(iops);
		setSizeGiB(sizeGiB);
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

	@DynamoDBAttribute(attributeName = "timeCreated")
	public String getTimeCreated() {
		return (String) attributes.get("timeCreated");
	}

	public void setTimeCreated(String timeCreated) {
		attributes.put("timeCreated", timeCreated);
	}

	@DynamoDBAttribute(attributeName = "size")
	public String getSize() {
		return (String) attributes.get("size");
	}

	public void setSize(String size) {
		attributes.put("size", size);
	}

	@DynamoDBAttribute(attributeName = "state")
	public String getState() {
		return (String) attributes.get("state");
	}

	public void setState(String state) {
		attributes.put("state", state);
	}

	@DynamoDBAttribute(attributeName = "instanceId")
	public String getInstanceId() {
		return (String) attributes.get("instanceId");
	}

	public void setInstanceId(String instanceId) {
		attributes.put("instanceId", instanceId);
	}

	@DynamoDBAttribute(attributeName = "snapshotId")
	public void setSnapsotId(String snapshotId) {
		attributes.put("snapshotId", snapshotId);
	}

	public String getSnapshotId() {
		return (String) attributes.get("snapshotId");
	}

	@DynamoDBAttribute(attributeName = "volumeType")
	public void setVolumeType(String volumeType) {
		attributes.put("volumeType", volumeType);
	}

	public String getVolumeType() {
		return (String) attributes.get("volumeType");
	}

	@DynamoDBAttribute(attributeName = "iops")
	public void setIops(String iops) {
		attributes.put("iops", iops);
	}

	public String getIops() {
		return (String) attributes.get("iops");
	}

	@DynamoDBAttribute(attributeName = "sizeGiB")
	public void setSizeGiB(String sizeGiB) {
		attributes.put("sizeGiB", sizeGiB);
	}

	public String getSizeGiB() {
		return (String) attributes.get("sizeGiB");
	}

	@Override
	public String toString() {
		return Jackson.toJsonString(attributes);
	}

}

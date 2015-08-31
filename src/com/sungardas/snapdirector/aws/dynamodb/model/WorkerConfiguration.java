package com.sungardas.snapdirector.aws.dynamodb.model;

import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBAttribute;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBHashKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBTable;
import com.amazonaws.services.dynamodbv2.document.Item;

@DynamoDBTable(tableName = "Configurations")
public class WorkerConfiguration {
	
	@DynamoDBAttribute(attributeName = "sdfsVolumeName")
	public String getSdfsVolumeName() {
		return sdfsVolumeName;
	}

	public void setSdfsVolumeName(String sdfsVolumeName) {
		this.sdfsVolumeName = sdfsVolumeName;
	}

	@DynamoDBAttribute(attributeName = "sdfsMountPoint")
	public String getSdfsMountPoint() {
		return sdfsMountPoint;
	}

	public void setSdfsMountPoint(String sdfsMountPoint) {
		this.sdfsMountPoint = sdfsMountPoint;
	}

	@DynamoDBAttribute(attributeName = "region")
	public String getEc2Region() {
		return region;
	}

	public void setEc2Region(String ec2Region) {
		this.region = ec2Region;
	}

	 @DynamoDBHashKey()
	public String getConfigurationId() {
		return configurationId;
	}

	public void setConfigurationId(String configurationId) {
		this.configurationId = configurationId;
	}

	@DynamoDBAttribute(attributeName = "taskQueueURL")
	public String getTaskQueueURL() {
		return taskQueueURL;
	}

	public void setTaskQueueURL(String taskQueueURL) {
		this.taskQueueURL = taskQueueURL;
	}

	@DynamoDBAttribute(attributeName = "useFakeBackup")
	public boolean isUseFakeBackup() {
		return useFakeBackup;
	}

	
	public void setUseFakeBackup(boolean useFakeBackup) {
		this.useFakeBackup = useFakeBackup;
	}

	@DynamoDBAttribute(attributeName = "useFakeEC2")
	public boolean isUseFakeEC2() {
		return useFakeEC2;
	}

	
	public void setUseFakeEC2(boolean useFakeEC2) {
		this.useFakeEC2 = useFakeEC2;
	}

	@DynamoDBAttribute(attributeName = "fakeBackupSource")
	public String getFakeBackupSource() {
		return fakeBackupSource;
	}

	
	public void setFakeBackupSource(String fakeBackupSource) {
		this.fakeBackupSource = fakeBackupSource;
	}

	private String sdfsVolumeName;
	private String sdfsMountPoint;
	
	private String region;
	private String configurationId;
	private String taskQueueURL;
	
	private boolean useFakeBackup;
	private boolean useFakeEC2;
	private String fakeBackupSource;
	
}

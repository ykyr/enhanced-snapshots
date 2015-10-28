package com.sungardas.enhancedsnapshots.aws.dynamodb.model;

import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBAttribute;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBHashKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBTable;

@DynamoDBTable(tableName = "Configurations")
public class WorkerConfiguration {

	private String sdfsVolumeName;
	private String sdfsMountPoint;
	private String region;
	private String configurationId;
	private String s3Bucket;

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

	@DynamoDBAttribute(attributeName = "s3Bucket")
	public String getS3Bucket() {
		return s3Bucket;
	}

	public void setS3Bucket(String taskS3Bucket) {
		this.s3Bucket = taskS3Bucket;
	}
	
}

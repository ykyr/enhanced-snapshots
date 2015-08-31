package com.sungardas.snapdirector.dto;

public class WorkerConfigurationDto {
	
	
	public String getTaskQueueURL() {
		return taskQueueURL;
	}
	public void setTaskQueueURL(String taskQueueURL) {
		this.taskQueueURL = taskQueueURL;
	}
	public String getWorkerId() {
		return workerId;
	}
	public void setWorkerId(String workerId) {
		this.workerId = workerId;
	}
	public boolean isUseFakeBackup() {
		return useFakeBackup;
	}
	public void setUseFakeBackup(boolean useFakeBackup) {
		this.useFakeBackup = useFakeBackup;
	}
	public boolean isUseFakeEC2() {
		return useFakeEC2;
	}
	public void setUseFakeEC2(boolean useFakeEC2) {
		this.useFakeEC2 = useFakeEC2;
	}
	public String getSdfsMountPoint() {
		return sdfsMountPoint;
	}
	public void setSdfsMountPoint(String sdfsMountPoint) {
		this.sdfsMountPoint = sdfsMountPoint;
	}
	public String getSdfsVolumeName() {
		return sdfsVolumeName;
	}
	public void setSdfsVolumeName(String sdfsVolumeName) {
		this.sdfsVolumeName = sdfsVolumeName;
	}
	public String getSdfsBucket() {
		return sdfsBucket;
	}
	public void setSdfsBucket(String sdfsBucket) {
		this.sdfsBucket = sdfsBucket;
	}
	
	// SQS url that used to send messages to worker engine
	private String taskQueueURL;
	
	//worker identifier
	//(instance id of ec2 instance where application runs / configuration identifier if runs on developers environment)
	private String workerId;
	
	// flag used by developers to test backup of small file instead of real volume
	private boolean useFakeBackup;
	
	// flag used by developers to test backup without ec2 snapshot manipulations
	private boolean useFakeEC2;
	
	// sdfs volume name
	private String sdfsVolumeName;
		
	// sdfs mount point
	private String sdfsMountPoint;
	
	// s3 bucket used by sdfs to store chunks
	private String sdfsBucket;
	

}

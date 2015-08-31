package com.sungardas.snapdirector.dto;


public class TaskDto {

	private String priority;
	private String status;
	private String type;
	private String volume;
	private String schedulerManual;
	private String schedulerName;
	private String schedulerTime;
	private String instanceId;
	private String backupFileName;

	public String getBackupFileName() {
		return backupFileName;
	}

	public void setBackupFileName(String backupFileName) {
		this.backupFileName = backupFileName;
	}

	public TaskDto() {
	}

	public String getPriority() {
		return priority;
	}

	public void setPriority(String priority) {
		this.priority = priority;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getVolume() {
		return volume;
	}

	public void setVolume(String volume) {
		this.volume = volume;
	}

	public String getSchedulerManual() {
		return schedulerManual;
	}

	public void setSchedulerManual(String schedulerManual) {
		this.schedulerManual = schedulerManual;
	}

	public String getSchedulerName() {
		return schedulerName;
	}

	public void setSchedulerName(String schedulerName) {
		this.schedulerName = schedulerName;
	}

	public String getSchedulerTime() {
		return schedulerTime;
	}

	public void setSchedulerTime(String schedulerTime) {
		this.schedulerTime = schedulerTime;
	}

	public String getInstanceId() {
		return instanceId;
	}

	public void setInstanceId(String instanceId) {
		this.instanceId = instanceId;
	}

}

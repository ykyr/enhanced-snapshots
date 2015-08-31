package com.sungardas.snapdirector.dto;

public class TaskDto {
	private String id;
	private String instanceid;
	private String options;
	private Integer priority;
	private Boolean schedulerManual;
	private String schedulerName;
	private String schedulerTime;
	private String status;
	private String type;
	private String volume;
	private String worker;
	
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public String getInstanceid() {
		return instanceid;
	}
	public void setInstanceid(String instanceid) {
		this.instanceid = instanceid;
	}
	public String getOptions() {
		return options;
	}
	public void setOptions(String options) {
		this.options = options;
	}
	public Integer getPriority() {
		return priority;
	}
	public void setPriority(Integer priority) {
		this.priority = priority;
	}
	public Boolean getSchedulerManual() {
		return schedulerManual;
	}
	public void setSchedulerManual(Boolean schedulerManual) {
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
	public String getWorker() {
		return worker;
	}
	public void setWorker(String worker) {
		this.worker = worker;
	}
	
	
}

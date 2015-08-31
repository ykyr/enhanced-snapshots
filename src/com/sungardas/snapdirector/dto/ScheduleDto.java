package com.sungardas.snapdirector.dto;

public class ScheduleDto {
	private String id;
	private String cron;
	private Boolean enabled;
	private String name;
	private Long nextFire;
	private String volumeId;
	
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public String getCron() {
		return cron;
	}
	public void setCron(String cron) {
		this.cron = cron;
	}
	public Boolean getEnabled() {
		return enabled;
	}
	public void setEnabled(Boolean enabled) {
		this.enabled = enabled;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public Long getNextFire() {
		return nextFire;
	}
	public void setNextFire(Long nextFire) {
		this.nextFire = nextFire;
	}
	public String getVolumeId() {
		return volumeId;
	}
	public void setVolumeId(String volumeId) {
		this.volumeId = volumeId;
	}
	
	
}

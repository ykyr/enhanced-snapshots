package com.sungardas.snapdirector.dto;

public class SnapshotDto {

	private String id;
	private String snapshotId;
	private String volumeId;
	
	public SnapshotDto(){}
	
	public SnapshotDto(String snapshotId, String volumeId){
		this.snapshotId = snapshotId;
		this.volumeId = volumeId;
	}
	
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public String getSnapshotId() {
		return snapshotId;
	}
	public void setSnapshotId(String snapshotId) {
		this.snapshotId = snapshotId;
	}
	public String getVolumeId() {
		return volumeId;
	}
	public void setVolumeId(String volumeId) {
		this.volumeId = volumeId;
	}
	
	
}

package com.sungardas.enhancedsnapshots.aws.dynamodb.model;

import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBHashKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBRangeKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBTable;

import org.springframework.data.annotation.Id;

@DynamoDBTable(tableName = "BackupList")
final public class BackupEntry {

    @Id
    private final BackupEntryId id = new BackupEntryId();

    private String timeCreated;
    private String size;
    private String state;
    private String snapshotId;
    private String volumeType;
    private String iops;
    private String sizeGiB;


	public BackupEntry() {

    }

	public BackupEntry(String volumeId, String fileName, String timeCreated, String backupSize, BackupState state,
                       String snapshotId, String volumeType, String iops, String sizeGiB) {
        setVolumeId(volumeId);
		setFileName(fileName);
		setTimeCreated(timeCreated);
		setSize(backupSize);
		setState(state.getState());
		setSnapshotId(snapshotId);
		setVolumeType(volumeType);
		setIops(iops);
		setSizeGiB(sizeGiB);
	}

    @DynamoDBHashKey(attributeName = "volumeId")
    public String getVolumeId() {
        return id.getVolumeId();
    }

    public void setVolumeId(final String volumeId) {
        id.setVolumeId(volumeId);
    }

    @DynamoDBRangeKey(attributeName = "fileName")
    public String getFileName() {
        return id.getFileName();
    }

    public void setFileName(final String fileName) {
        id.setFileName(fileName);
    }

    public String getTimeCreated() {
        return timeCreated;
    }

    public void setTimeCreated(final String timeCreated) {
        this.timeCreated = timeCreated;
    }

    public String getSize() {
        return size;
    }

    public void setSize(final String size) {
        this.size = size;
    }

    public String getState() {
        return state;
    }

    public void setState(final String state) {
        this.state = state;
    }

    public String getSnapshotId() {
        return snapshotId;
    }

    public void setSnapshotId(final String snapshotId) {
        this.snapshotId = snapshotId;
    }

    public String getVolumeType() {
        return volumeType;
    }

    public void setVolumeType(final String volumeType) {
        this.volumeType = volumeType;
    }

    public String getIops() {
        return iops;
    }

    public void setIops(final String iops) {
        this.iops = iops;
    }

    public String getSizeGiB() {
        return sizeGiB;
    }

    public void setSizeGiB(final String sizeGiB) {
        this.sizeGiB = sizeGiB;
    }
}

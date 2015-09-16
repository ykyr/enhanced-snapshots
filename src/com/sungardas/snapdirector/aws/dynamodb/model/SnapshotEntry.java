package com.sungardas.snapdirector.aws.dynamodb.model;

import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBAttribute;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBHashKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBTable;

@DynamoDBTable(tableName = "Snapshots")
public class SnapshotEntry {

    @DynamoDBAttribute
    private String instanceId;

    @DynamoDBAttribute
    private String snapshotId;

    @DynamoDBAttribute
    private String volumeId;


    public SnapshotEntry() {
    }

    public SnapshotEntry(String instanceId, String snapshotId, String volumeId) {
        this.instanceId = instanceId;
        this.snapshotId = snapshotId;
        this.volumeId = volumeId;
    }

    public String getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(String instanceId) {
        this.instanceId = instanceId;
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

    @DynamoDBHashKey
    public String getVolumeInstanceId() {
        return volumeId + ":" + instanceId;
    }

    public void setVolumeInstanceId(String instanceId) {
        //skip
    }
}

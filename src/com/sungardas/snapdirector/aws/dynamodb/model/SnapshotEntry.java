package com.sungardas.snapdirector.aws.dynamodb.model;

import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBAttribute;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBHashKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBRangeKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBTable;

@DynamoDBTable(tableName = "Snapshots")
public class SnapshotEntry {

    @DynamoDBAttribute
    private String instanceId;

    @DynamoDBAttribute
    private String snapshotId;

    @DynamoDBHashKey
    private String volumeInstanceId;

    public String getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(String instanceId) {
        this.instanceId = instanceId;
    }

    public SnapshotEntry() {}

    public SnapshotEntry(String snapshotId, String volumeId, String instanceId) {
        setSnapshotId(snapshotId);
        setVolumeInstanceId(volumeId, instanceId);

    }

    public String getSnapshotId() {
        return snapshotId;
    }

    public void setSnapshotId(String snapshotId) {
        this.snapshotId = snapshotId;
    }


    public String getVolumeInstanceId() {
        return volumeInstanceId;
    }

    public void setVolumeInstanceId(String volumeId, String instanceId) {
        this.volumeInstanceId = volumeId + ":" +instanceId;
    }
}

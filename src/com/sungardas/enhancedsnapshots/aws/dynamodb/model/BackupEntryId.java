package com.sungardas.enhancedsnapshots.aws.dynamodb.model;

import java.io.Serializable;

import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBHashKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBRangeKey;

public class BackupEntryId implements Serializable {

    @DynamoDBHashKey(attributeName = "volumeId")
    private String volumeId;

    @DynamoDBRangeKey(attributeName = "fileName")
    private String fileName;

    public BackupEntryId() {
    }

    public BackupEntryId(final String volumeId, final String fileName) {
        this.volumeId = volumeId;
        this.fileName = fileName;
    }

    public String getVolumeId() {
        return volumeId;
    }

    public void setVolumeId(final String volumeId) {
        this.volumeId = volumeId;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(final String fileName) {
        this.fileName = fileName;
    }
}

package com.sungardas.enhancedsnapshots.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Collections;
import java.util.List;

public class InitConfigurationDto {
    private List<S3> s3 = Collections.EMPTY_LIST;
    private SDFS sdfs;
    private Queue queue;
    private DB db;

    public List<S3> getS3() {
        return s3;
    }

    public void setS3(List<S3> s3) {
        this.s3 = s3;
    }

    public SDFS getSdfs() {
        return sdfs;
    }

    public void setSdfs(SDFS sdfs) {
        this.sdfs = sdfs;
    }

    public Queue getQueue() {
        return queue;
    }

    public void setQueue(Queue queue) {
        this.queue = queue;
    }

    public DB getDb() {
        return db;
    }

    public void setDb(DB db) {
        this.db = db;
    }

    public static class S3 {
        @JsonProperty("isCreated")
        private boolean created;
        private String bucketName;

        public S3(String bucketName, boolean created) {
            this.created = created;
            this.bucketName = bucketName;
        }

        public S3() {

        }

        public boolean isCreated() {
            return created;
        }

        public void setCreated(boolean created) {
            this.created = created;
        }

        public String getBucketName() {
            return bucketName;
        }

        public void setBucketName(String bucketName) {
            this.bucketName = bucketName;
        }
    }

    public static class SDFS {
        @JsonProperty("isCreated")
        private boolean created;
        private String volumeName;
        private String volumeSize;
        private String mountPoint;

        public boolean isCreated() {
            return created;
        }

        public void setCreated(boolean created) {
            this.created = created;
        }

        public String getVolumeName() {
            return volumeName;
        }

        public void setVolumeName(String volumeName) {
            this.volumeName = volumeName;
        }

        public String getVolumeSize() {
            return volumeSize;
        }

        public void setVolumeSize(String volumeSize) {
            this.volumeSize = volumeSize;
        }

        public String getMountPoint() {
            return mountPoint;
        }

        public void setMountPoint(String mountPoint) {
            this.mountPoint = mountPoint;
        }
    }

    public static class Queue {
        @JsonProperty("isCreated")
        private boolean created;
        private String queueName;

        public boolean isCreated() {
            return created;
        }

        public void setCreated(boolean created) {
            this.created = created;
        }

        public String getQueueName() {
            return queueName;
        }

        public void setQueueName(String queueName) {
            this.queueName = queueName;
        }
    }

    public static class DB {
        @JsonProperty("isValid")
        private boolean valid;

        @JsonProperty("hasAdmin")
        private boolean adminExist;

        public boolean isAdminExist() {
            return adminExist;
        }

        public void setAdminExist(boolean adminExist) {
            this.adminExist = adminExist;
        }

        public boolean isValid() {
            return valid;
        }

        public void setValid(boolean valid) {
            this.valid = valid;
        }
    }
}

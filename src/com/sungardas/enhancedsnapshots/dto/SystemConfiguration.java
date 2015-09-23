package com.sungardas.enhancedsnapshots.dto;


public class SystemConfiguration {

    private S3 s3;
    private SDFS sdfs;
    private Queue queue;
    private Long lastBackup;
    private EC2Instance ec2Instance;


    public EC2Instance getEc2Instance() {
        return ec2Instance;
    }

    public void setEc2Instance(EC2Instance ec2Instance) {
        this.ec2Instance = ec2Instance;
    }

    public SDFS getSdfs() {
        return sdfs;
    }

    public void setSdfs(SDFS sdfs) {
        this.sdfs = sdfs;
    }

    public S3 getS3() {
        return s3;
    }

    public void setS3(S3 s3) {
        this.s3 = s3;
    }


    public Long getLastBackup() {
        return lastBackup;
    }

    public void setLastBackup(Long lastBackup) {
        this.lastBackup = lastBackup;
    }

    public Queue getQueue() {
        return queue;
    }

    public void setQueue(Queue queue) {
        this.queue = queue;
    }


    public static class S3 {
        public String getBucketName() {
            return bucketName;
        }

        private String bucketName;

        public void setBucketName(String bucketName) {
            this.bucketName = bucketName;
        }
    }

    public static class SDFS {
        private String volumeName;
        private String volumeSize;
        private String mountPoint;

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
        private String queueName;

        public String getQueueName() {
            return queueName;
        }

        public void setQueueName(String queueName) {
            this.queueName = queueName;
        }
    }

    public static class EC2Instance {
        public String getInstanceID() {
            return instanceID;
        }

        public void setInstanceID(String instanceID) {
            this.instanceID = instanceID;
        }

        private String instanceID;
    }

}

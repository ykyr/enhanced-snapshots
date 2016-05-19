package com.sungardas.enhancedsnapshots.dto;


import com.amazonaws.services.ec2.model.VolumeType;


public class SystemConfiguration {

    private S3 s3;
    private SDFS sdfs;
    private Long lastBackup;
    private EC2Instance ec2Instance;
    private SystemProperties systemProperties;
    private String currentVersion;
    private String latestVersion;


    public String getCurrentVersion() {
        return currentVersion;
    }

    public SystemProperties getSystemProperties() {
        return systemProperties;
    }

    public void setSystemProperties(SystemProperties systemProperties) {
        this.systemProperties = systemProperties;
    }

    public void setCurrentVersion(final String currentVersion) {
        this.currentVersion = currentVersion;
    }

    public String getLatestVersion() {
        return latestVersion;
    }

    public void setLatestVersion(final String latestVersion) {
        this.latestVersion = latestVersion;
    }

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

    public static class S3 {
        private String bucketName;

        public String getBucketName() {
            return bucketName;
        }

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

    public static class EC2Instance {
        private String instanceID;

        public String getInstanceID() {
            return instanceID;
        }

        public void setInstanceID(String instanceID) {
            this.instanceID = instanceID;
        }
    }

    public static class SystemProperties {

        private String tempVolumeType;
        private int tempVolumeIopsPerGb;
        private String restoreVolumeType;
        private int restoreVolumeIopsPerGb;
        private String[] volumeTypeOptions;

        public String getTempVolumeType() {
            return tempVolumeType;
        }

        public void setTempVolumeType(String tempVolumeType) {
            this.tempVolumeType = tempVolumeType;
        }

        public int getTempVolumeIopsPerGb() {
            return tempVolumeIopsPerGb;
        }

        public void setTempVolumeIopsPerGb(int tempVolumeIopsPerGb) {
            this.tempVolumeIopsPerGb = tempVolumeIopsPerGb;
        }

        public String getRestoreVolumeType() {
            return restoreVolumeType;
        }

        public void setRestoreVolumeType(String restoreVolumeType) {
            this.restoreVolumeType = restoreVolumeType;
        }

        public int getRestoreVolumeIopsPerGb() {
            return restoreVolumeIopsPerGb;
        }

        public void setRestoreVolumeIopsPerGb(int restoreVolumeIopsPerGb) {
            this.restoreVolumeIopsPerGb = restoreVolumeIopsPerGb;
        }

        public String[] getVolumeTypeOptions() {
            return volumeTypeOptions;
        }

        public void setVolumeTypeOptions(String[] volumeTypeOptions) {
            this.volumeTypeOptions = volumeTypeOptions;
        }
    }
}

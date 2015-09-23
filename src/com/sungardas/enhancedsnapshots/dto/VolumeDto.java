package com.sungardas.enhancedsnapshots.dto;

import java.util.Date;
import java.util.List;

import com.amazonaws.services.ec2.model.Tag;

public class VolumeDto {

    private String volumeName;
    private String volumeId;
    private int size;
    private String snapshotId;
    private Date createTime;
    private String availabilityZone;
    private String state;
    private boolean scheduled;
    private List<Tag> tags;

    public String getInstanceID() {
        return instanceID;
    }

    public void setInstanceID(String instanceID) {
        this.instanceID = instanceID;
    }

    private String instanceID;

    public String getVolumeName() {
        return volumeName;
    }

    public void setVolumeName(String volumeName) {
        this.volumeName = volumeName;
    }

    public String getVolumeId() {
        return volumeId;
    }

    public void setVolumeId(String volumeId) {
        this.volumeId = volumeId;
    }

    public int getSize() {
        return size;
    }

    public void setSize(int size) {
        this.size = size;
    }

    public String getSnapshotId() {
        return snapshotId;
    }

    public void setSnapshotId(String snapshotId) {
        this.snapshotId = snapshotId;
    }

    public Date getCreateTime() {
        return createTime;
    }

    public void setCreateTime(Date createTime) {
        this.createTime = createTime;
    }

    public String getAvailabilityZone() {
        return availabilityZone;
    }

    public void setAvailabilityZone(String availabilityZone) {
        this.availabilityZone = availabilityZone;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public List<Tag> getTags() {
        return tags;
    }

    public void setTags(List<Tag> tags) {
        this.tags = tags;
    }

    public boolean isScheduled() {
        return scheduled;
    }

    public void setScheduled(final boolean scheduled) {
        this.scheduled = scheduled;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        final VolumeDto volumeDto = (VolumeDto) o;

        if (size != volumeDto.size) {
            return false;
        }
        if (scheduled != volumeDto.scheduled) {
            return false;
        }
        if (volumeName != null ? !volumeName.equals(volumeDto.volumeName) : volumeDto.volumeName != null) {
            return false;
        }
        if (volumeId != null ? !volumeId.equals(volumeDto.volumeId) : volumeDto.volumeId != null) {
            return false;
        }
        if (snapshotId != null ? !snapshotId.equals(volumeDto.snapshotId) : volumeDto.snapshotId != null) {
            return false;
        }
        if (createTime != null ? !createTime.equals(volumeDto.createTime) : volumeDto.createTime != null) {
            return false;
        }
        if (availabilityZone != null ? !availabilityZone.equals(volumeDto.availabilityZone) : volumeDto.availabilityZone != null) {
            return false;
        }
        if (state != null ? !state.equals(volumeDto.state) : volumeDto.state != null) {
            return false;
        }
        if (tags != null ? !tags.equals(volumeDto.tags) : volumeDto.tags != null) {
            return false;
        }
        return !(instanceID != null ? !instanceID.equals(volumeDto.instanceID) : volumeDto.instanceID != null);

    }

    @Override
    public int hashCode() {
        int result = volumeName != null ? volumeName.hashCode() : 0;
        result = 31 * result + (volumeId != null ? volumeId.hashCode() : 0);
        result = 31 * result + size;
        result = 31 * result + (snapshotId != null ? snapshotId.hashCode() : 0);
        result = 31 * result + (createTime != null ? createTime.hashCode() : 0);
        result = 31 * result + (availabilityZone != null ? availabilityZone.hashCode() : 0);
        result = 31 * result + (state != null ? state.hashCode() : 0);
        result = 31 * result + (scheduled ? 1 : 0);
        result = 31 * result + (tags != null ? tags.hashCode() : 0);
        result = 31 * result + (instanceID != null ? instanceID.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "VolumeDto{" +
                "volumeName='" + volumeName + '\'' +
                ", volumeId='" + volumeId + '\'' +
                ", size=" + size +
                ", snapshotId='" + snapshotId + '\'' +
                ", createTime=" + createTime +
                ", availabilityZone='" + availabilityZone + '\'' +
                ", state='" + state + '\'' +
                ", scheduled=" + scheduled +
                ", tags=" + tags +
                ", instanceID='" + instanceID + '\'' +
                '}';
    }
}




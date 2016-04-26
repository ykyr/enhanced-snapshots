package com.sungardas.enhancedsnapshots.service.impl;

import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.model.*;
import com.sungardas.enhancedsnapshots.exception.EnhancedSnapshotsException;
import com.sungardas.enhancedsnapshots.service.AWSCommunicationService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static java.lang.String.format;

@Service
@Profile("prod")
public class AWSCommunicationServiceImpl implements AWSCommunicationService {

    private static final Logger LOG = LogManager.getLogger(AWSCommunicationServiceImpl.class);
    private static final String AVAILABLE_STATE = VolumeState.Available.toString();

    @Value("${enhancedsnapshots.amazon.wait.time.before.new.sync:15}")
    private int WAIT_TIME_BEFORE_NEXT_CHECK_IN_SECONDS;

    @Value("${enhancedsnapshots.amazon.max.wait.time.to.detach.volume:300}")
    private int MAX_WAIT_TIME_VOLUME_TO_DETACH_IN_SECONDS;

    @Autowired
    private AmazonEC2 ec2client;

    @Value("${sungardas.worker.configuration}")
    private String configurationId;

   @Override
   public List<AvailabilityZone> describeAvailabilityZonesForCurrentRegion() {
       return ec2client.describeAvailabilityZones().getAvailabilityZones();
   }

    @Override
    public String getCurrentAvailabilityZone() {
        return getInstance(configurationId).getPlacement()
                .getAvailabilityZone();    }

    @Override
    public void createTemporaryTag(String resourceId, String description) {
        CreateTagsRequest tagsRequest = new CreateTagsRequest().withResources(resourceId).withTags(
                new Tag().withKey("ESTempVolume").withValue(description));
        ec2client.createTags(tagsRequest);
    }

    @Override
    public void deleteTemporaryTag(String resourceId) {
        List<Volume> volumes= ec2client.describeVolumes(new DescribeVolumesRequest().withVolumeIds(resourceId)).getVolumes();
        if(volumes.size()>0) {
            Volume volume = volumes.get(0);
            List<Tag> tags = volume.getTags();
            boolean tagWasDeleted = false;
            for(Tag tag: tags) {
                if (tag.getKey().equals("ESTempVolume")) {
                    DeleteTagsRequest tagsRequest = new DeleteTagsRequest().withResources(resourceId).withTags();
                    ec2client.deleteTags(tagsRequest);
                    tagWasDeleted = true;
                    break;
                }
            }
            if(! tagWasDeleted) {
                LOG.info("No temporary tag associated with volume {}",resourceId);
            }
        } else {
            LOG.info("Volume with id {} does not exist ",resourceId);
        }

    }

    @Override
    public Volume createVolume(int size, int iiops, String type) {
        if (type.equals("standard")) type="gp2";
	String availabilityZone = getInstance(configurationId).getPlacement()
                .getAvailabilityZone();

        CreateVolumeRequest createVolumeRequest = new CreateVolumeRequest()
                .withSize(size).withVolumeType(type)
                .withAvailabilityZone(availabilityZone);
        if (iiops > 0) {
            createVolumeRequest = createVolumeRequest.withIops(iiops);
        }
        Volume result = ec2client.createVolume(createVolumeRequest).getVolume();
        return result;
    }

    @Override
    public Volume createStandardVolume(int size) {
        return createVolume(size, 0, "standard");
    }

    @Override
    public Volume createGP2Volume(int size) {
        return createVolume(size, 0, "gp2");
    }

    @Override
    public Volume createIO1Volume(int size, int iops) {
        return createVolume(size, iops, "io1");
    }

    @Override
    public Snapshot createSnapshot(Volume volume) {
        SimpleDateFormat formatter = new SimpleDateFormat(
                "yyyy.MM.dd'_T'hh:mm:ss");

        String volumeId = volume.getVolumeId();
        LOG.info(format("Starting creating snapshot for %s", volumeId));
        CreateSnapshotRequest snapshotRequest = new CreateSnapshotRequest(
                volumeId,
                volumeId
                        + "__"
                        + formatter.format(new Date(System.currentTimeMillis())));
        CreateSnapshotResult crSnapshotResult = ec2client
                .createSnapshot(snapshotRequest);
        Snapshot snapshot = crSnapshotResult.getSnapshot();
        return snapshot;
    }

    @Override
    public void deleteSnapshot(String snapshotId) {
        LOG.info(format("Deleting snapshot: %s", snapshotId));
        DeleteSnapshotRequest deleteSnapshotRequest = new DeleteSnapshotRequest();
        deleteSnapshotRequest.setSnapshotId(snapshotId);
        try {
            ec2client.deleteSnapshot(deleteSnapshotRequest);
        } catch (Throwable e) {
            LOG.info("Snapshot with id {} does not exist ", snapshotId);
        }
    }

    @Override
    public void cleanupSnapshots(String volumeId, String snapshotIdToLeave) {
        deleteSnapshot(snapshotIdToLeave);
    }

    @Override
    public Snapshot waitForCompleteState(Snapshot snapshot) {
        Snapshot syncSnapshot;
        do {
            try {
                TimeUnit.SECONDS.sleep(WAIT_TIME_BEFORE_NEXT_CHECK_IN_SECONDS);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            syncSnapshot = syncSnapshot(snapshot.getSnapshotId());
            LOG.debug("Snapshot state: {}, progress: {}", syncSnapshot.getState(), syncSnapshot.getProgress());
            if (syncSnapshot.getState().equals(SnapshotState.Error)) {
                new AWSCommunicationServiceException("Snapshot " + snapshot.getSnapshotId() + " is in error state");
            }
        } while (syncSnapshot.getState().equals(SnapshotState.Pending) || !syncSnapshot.getProgress().equals("100%"));
        return syncSnapshot;
    }

    @Override
    public Snapshot syncSnapshot(String snapshotId) {
        Snapshot syncSnapshot = getSnapshot(snapshotId);
        if (syncSnapshot != null) {
            return syncSnapshot;
        }
        LOG.error("Failed to sync snapshot {}. Snapshot does not exist.", snapshotId);
        throw new AWSCommunicationServiceException("Can not sync snapshot. Snapshot " + snapshotId + " does not exist.");
    }

    @Override
    public Volume waitForAvailableState(Volume volume) {
        String state;
        Volume result;
        do {
            try {
                TimeUnit.SECONDS.sleep(WAIT_TIME_BEFORE_NEXT_CHECK_IN_SECONDS);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            result = syncVolume(volume);
            state = result.getState();
            System.out.println("waitForAvailableState.current state: " + state);
            if (state.equals(VolumeState.Error.toString())) {
                throw new RuntimeException("error...");
            }
        } while (!state.equals(VolumeState.Available.toString())
                && !state.equals(VolumeState.Deleted.toString()));
        return result;
    }

    @Override
    public Volume getVolume(String volumeId) {
        DescribeVolumesRequest describeVolumesRequest = new DescribeVolumesRequest().withVolumeIds(volumeId);
        DescribeVolumesResult result = ec2client.describeVolumes(describeVolumesRequest);
        if (result.getVolumes().size() == 1) {
            return result.getVolumes().get(0);
        }
        return null;
    }


    private Snapshot getSnapshot(String snapshotId) {
        DescribeSnapshotsRequest describeSnapshotsRequest = new DescribeSnapshotsRequest().withSnapshotIds(snapshotId);
        DescribeSnapshotsResult describeSnapshotsResult = ec2client.describeSnapshots(describeSnapshotsRequest);
        if (describeSnapshotsResult.getSnapshots().size() == 1) {
            return describeSnapshotsResult.getSnapshots().get(0);
        }
        return null;
    }

    @Override
    public Instance getInstance(String instanceId) {
        DescribeInstancesRequest describeInstancesRequest = new DescribeInstancesRequest()
                .withInstanceIds(instanceId);
        DescribeInstancesResult describeInstancesResult = ec2client
                .describeInstances(describeInstancesRequest);
        List<Reservation> reservations = describeInstancesResult
                .getReservations();
        for (Reservation res : reservations) {
            if (res.getInstances().size() > 0) {
                return res.getInstances().get(0);
            }
        }
        return null;
    }

    @Override
    public void detachVolume(Volume volume) {
        DetachVolumeRequest detachVolumeRequest = new DetachVolumeRequest(volume.getVolumeId());
        ec2client.detachVolume(detachVolumeRequest);
        waitVolumeToDetach(volume);
    }

    @Override
    public Volume createVolumeFromSnapshot(String snapshotId,
                                           String availabilityZoneName) {
        String type="gp2";
	CreateVolumeRequest crVolumeRequest = new CreateVolumeRequest()
                .withVolumeType(type)
		.withSnapshotId(snapshotId)
                .withAvailabilityZone(availabilityZoneName);
	//CreateVolumeRequest crVolumeRequest = new CreateVolumeRequest(
        //        snapshotId, availabilityZoneName);
        CreateVolumeResult crVolumeResult = ec2client
                .createVolume(crVolumeRequest);
        return crVolumeResult.getVolume();
    }

    @Override
    public Volume createVolumeFromSnapshot(Snapshot snapshot,
                                           String availabilityZoneName) {
        return createVolumeFromSnapshot(snapshot.getSnapshotId(),
                availabilityZoneName);
    }

    @Override
    public Volume syncVolume(Volume volume) {
        Volume syncVolume = getVolume(volume.getVolumeId());
        if (syncVolume != null) {
            return syncVolume;
        }
        LOG.error("Failed to sync volume {}. Volume does not exist.", volume.getVolumeId());
        throw new AWSCommunicationServiceException("Can not sync volume. Volume " + volume.getVolumeId() + " does not exist.");
    }

    @Override
    public void deleteVolume(Volume volume) {
        DeleteVolumeRequest deleteVolumeRequest = new DeleteVolumeRequest(volume.getVolumeId());
        ec2client.deleteVolume(deleteVolumeRequest);
        LOG.info(format("Volume %s deleted", volume.getVolumeId()));
    }

    @Override
    public void attachVolume(Instance instance, Volume volume) {
        String deviceName = getNextAvailableDeviceName(instance);
        AttachVolumeRequest attachVolumeRequest = new AttachVolumeRequest(volume.getVolumeId(),
                instance.getInstanceId(), deviceName);
        AttachVolumeResult res = ec2client.attachVolume(attachVolumeRequest);
        LOG.info(format("\nVolume attached. check instance data\n %s", instance.toString()));

    }

    private String getNextAvailableDeviceName(Instance instance) {

        List<InstanceBlockDeviceMapping> devList = instance
                .getBlockDeviceMappings();
        char lastChar = 'a';
        for (InstanceBlockDeviceMapping map : devList) {
            char ch = map.getDeviceName().charAt(map.getDeviceName().length() - 1);
            if (ch > lastChar) {
                lastChar = ch;
            }
        }
        if (lastChar < 'p' && lastChar >= 'f') {
            lastChar++;
            return "/dev/sd" + (char) lastChar;
        }
        return "/dev/sdf";
    }

    @Override
    public void setResourceName(String resourceId, String value) {
        addTag(resourceId, "Name", value);
    }

    @Override
    public void addTag(String resourceId, String name, String value) {
        CreateTagsRequest r = new CreateTagsRequest().withResources(resourceId)
                .withTags(new Tag().withKey(name).withValue(value));
        ec2client.createTags(r);
    }

    @Override
    public boolean snapshotExists(String snapshotId) {
        Snapshot snapshot = getSnapshot(snapshotId);
        if (snapshot != null) {
            return true;
        }
        return false;
    }

    @Override
    public boolean volumeExists(String volumeId) {
        if (getVolume(volumeId) != null) {
            return true;
        }
        return false;
    }

    private void waitVolumeToDetach(Volume volume) {
        int waitTime = 0;
        while (!volume.getState().equals(AVAILABLE_STATE) && waitTime < MAX_WAIT_TIME_VOLUME_TO_DETACH_IN_SECONDS) {
            try {
                LOG.debug("Volume {} is attached to {}", volume.getVolumeId(), volume.getAttachments().get(0).getInstanceId());
                TimeUnit.SECONDS.sleep(WAIT_TIME_BEFORE_NEXT_CHECK_IN_SECONDS);
                volume = syncVolume(volume);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            waitTime += WAIT_TIME_BEFORE_NEXT_CHECK_IN_SECONDS;
        }
        if (syncVolume(volume).getState().equals(AVAILABLE_STATE)) {
            LOG.debug("Volume {} detached.", volume.getVolumeId());
            return;
        }
        LOG.error("Failed to detach volume {}.", volume.getVolumeId());
        throw new AWSCommunicationServiceException("Failed to detach volume " + volume.getVolumeId());
    }

    public static class AWSCommunicationServiceException extends EnhancedSnapshotsException {
        public AWSCommunicationServiceException(String message) {
            super(message);
        }
    }

}

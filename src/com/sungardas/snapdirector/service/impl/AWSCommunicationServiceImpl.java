package com.sungardas.snapdirector.service.impl;

import com.amazonaws.AmazonClientException;
import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.model.*;
import com.sungardas.snapdirector.service.AWSCommunicationService;
import com.sungardas.snapdirector.service.SnapshotService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static java.lang.String.format;

@Service
public class AWSCommunicationServiceImpl implements AWSCommunicationService {

    private static final Logger LOG = LogManager
            .getLogger(AWSCommunicationServiceImpl.class);

    @Autowired
    private SnapshotService snapshotService;

    @Autowired
    private AmazonEC2 ec2client;

    @Value("${sungardas.worker.configuration}")
    private String configurationId;

    @Value("${sungardas.restore.snapshot.attempts:30}")
    private int retryRestoreAttempts;

    @Value("${sungardas.restore.snapshot.timeout:30}")
    private int retryRestoreTimeout;

    @Override
    public Volume createVolume(int size, int iiops, String type) {
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
        snapshotService.addSnapshot(snapshot.getSnapshotId(), volumeId);
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
        } finally {
            snapshotService.removeSnapshot(snapshotId);
        }
    }

    @Override
    public void cleanupSnapshots(String volumeId, String snapshotIdToLeave) {
        deleteSnapshot(snapshotIdToLeave);
    }

    @Override
    public Snapshot waitForCompleteState(Snapshot snapshot) {
        String state;
        Snapshot result;
        do {
            try {
                TimeUnit.SECONDS.sleep(20);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            result = syncSnapshot(snapshot);
            state = result.getState();
            if (state.equals(SnapshotState.Error)) {
                // TODO:exception
            }
        } while (state.equals(SnapshotState.Pending));

        return result;
    }

    @Override
    public Snapshot syncSnapshot(Snapshot snapshot) {
        DescribeSnapshotsRequest describeSnapshotsRequest = new DescribeSnapshotsRequest();
        LinkedList<String> ids = new LinkedList<String>();
        ids.add(snapshot.getSnapshotId());
        describeSnapshotsRequest.setSnapshotIds(ids);
        DescribeSnapshotsResult describeSnapshotsResult = ec2client
                .describeSnapshots(describeSnapshotsRequest);
        return describeSnapshotsResult.getSnapshots().get(0);
    }

    @Override
    public Volume waitForAvailableState(Volume volume) {
        String state;
        Volume result;
        do {
            try {
                TimeUnit.SECONDS.sleep(25);
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
        DescribeVolumesRequest describeVolumesRequest = new DescribeVolumesRequest();
        LinkedList<String> ids = new LinkedList<>();
        ids.add(volumeId);
        describeVolumesRequest.setVolumeIds(ids);
        DescribeVolumesResult describeVolumesResult = ec2client
                .describeVolumes(describeVolumesRequest);
        return describeVolumesResult.getVolumes().get(0);
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
        boolean incorrectState = true;
        long timeout = 10L;
        while (incorrectState) {
            try {
                incorrectState = false;
                DetachVolumeRequest detachVolumeRequest = new DetachVolumeRequest(volume.getVolumeId());
                DetachVolumeResult detachVolumeResult = ec2client.detachVolume(detachVolumeRequest);
            } catch (AmazonClientException incorrectStateException) {
                LOG.info(incorrectStateException.getMessage() + "\n Waiting for new try");
                incorrectState = true;
                timeout += timeout < 120 ? timeout * 2 : 0;
                try {
                    TimeUnit.SECONDS.sleep(timeout);
                } catch (InterruptedException e) {
                }
            }
        }
        LOG.info(format("\nVolume %s unattached", volume.getVolumeId()));
    }

    @Override
    public Volume createVolumeFromSnapshot(String snapshotId,
                                           String availabilityZoneName) {
        CreateVolumeRequest crVolumeRequest = new CreateVolumeRequest(
                snapshotId, availabilityZoneName);
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
        DescribeVolumesRequest describeVolumesRequest = new DescribeVolumesRequest()
                .withVolumeIds(volume.getVolumeId());
        DescribeVolumesResult result = ec2client
                .describeVolumes(describeVolumesRequest);
        return result.getVolumes().get(0);
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
        CreateTagsRequest r = new CreateTagsRequest().withResources(resourceId)
                .withTags(new Tag().withKey("Name").withValue(value));
        ec2client.createTags(r);
    }


    int getRetryRestoreAttempts() {
        return retryRestoreAttempts;
    }

    void setRetryRestoreAttempts(int retryRestoreAttempts) {
        this.retryRestoreAttempts = retryRestoreAttempts;
    }

    void setRetryRestoreTimeout(int retryRestoreTimeout) {
        this.retryRestoreTimeout = retryRestoreTimeout;
    }

}
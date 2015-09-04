package com.sungardas.snapdirector.tasks;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.model.*;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupState;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.aws.dynamodb.repository.BackupRepository;
import com.sungardas.snapdirector.aws.dynamodb.repository.TaskRepository;
import com.sungardas.snapdirector.service.AWSCommunticationService;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.StorageService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry.TaskEntryStatus.RUNNING;
import static java.lang.String.format;

@Component
@Scope("prototype")
@Profile("prod")
public class AWSBackupVolumeTask implements BackupTask {
    private static final Logger LOG = LogManager.getLogger(AWSBackupVolumeTask.class);
    @Autowired
    private TaskRepository taskRepository;

    @Autowired
    AmazonEC2 ec2client;

    @Autowired
    StorageService storageService;

    @Autowired
    BackupRepository backupRepository;

    @Autowired
    private AWSCommunticationService awsCommunication;

    private TaskEntry taskEntry;

    @Autowired
    private ConfigurationService configurationService;

    private WorkerConfiguration configuration;

    public void setTaskEntry(TaskEntry taskEntry) {
        this.taskEntry = taskEntry;
    }

    public void execute() {
        String volumeId = taskEntry.getVolume();
        configuration = configurationService.getConfiguration();

        LOG.info(format("AWSBackupVolumeTask: Starting backup process for volume %s", volumeId));
        LOG.info("Task " + taskEntry.getId() + ": Change task state to 'inprogress'");
        taskEntry.setStatus(RUNNING.getStatus());
        taskRepository.save(taskEntry);

        Volume tempVolume = null;
        String attachedDeviceName = null;

        tempVolume = createAndAttachBackupVolume(volumeId, configuration.getConfigurationId());
        try {
            TimeUnit.MINUTES.sleep(1);
        } catch (InterruptedException e1) {
            LOG.error(e1);
        }
        attachedDeviceName = storageService.detectFsDevName(tempVolume);

        String backupDate = String.valueOf(System.currentTimeMillis());
        String backupfileName = volumeId + "." + backupDate + ".backup";

        Volume volumeToBackup = awsCommunication.getVolume(volumeId);
        String snapshotId = tempVolume.getSnapshotId();
        String volumeType = volumeToBackup.getVolumeType();
        String iops = (volumeToBackup.getIops() != null) ? volumeToBackup.getIops().toString() : "";
        String sizeGib = tempVolume.getSize().toString();

        BackupEntry backup = new BackupEntry(volumeId, backupfileName, backupDate, "", BackupState.INPROGRESS,
                configuration.getConfigurationId(), snapshotId, volumeType, iops, sizeGib);
        backupRepository.save(backup);

        boolean backupStatus = false;
        try {
            String source = attachedDeviceName;
            LOG.info("Starting copying: " + source + " to:" + backupfileName);
            storageService.javaBinaryCopy(source, configuration.getSdfsMountPoint() + backupfileName);

            backupStatus = true;
        } catch (IOException | InterruptedException e) {
            LOG.fatal(format("Backup of volume %s failed", volumeId));
            backup.setState(BackupState.FAILED.getState());
            backupRepository.save(backup);
        }

        if (backupStatus) {
            long backupSize = storageService.getSize(configuration.getSdfsMountPoint() + backupfileName);
            long backupCreationtime = storageService.getBackupCreationTime(configuration.getSdfsMountPoint() + backupfileName);
            LOG.info("Backup creation time: " + backupCreationtime);
            LOG.info("Backup size: " + backupSize);

            LOG.info("Put backup entry to the Backup List: " + backup.toString());
            backup.setState(BackupState.COMPLETED.getState());
            backup.setSize(String.valueOf(backupSize));
            backupRepository.save(backup);
        }

        LOG.info("Detaching volume" + tempVolume.getVolumeId());
        awsCommunication.detachVolume(tempVolume);
        LOG.info("Deleting temporary volume" + tempVolume.getVolumeId());
        awsCommunication.deleteVolume(tempVolume);

        LOG.info("Cleaning up previously created snapshots");
        awsCommunication.cleanupSnapshots(volumeId, snapshotId);
        
        LOG.info(format("Backup process for volume %s finished successfully ", volumeId));
        LOG.info("Task " + taskEntry.getId() + ": Delete completed task:" + taskEntry.getId());
        taskRepository.delete(taskEntry);
        LOG.info("Task completed.");
    }

    //TODO: review this quick fix

    private Volume createAndAttachBackupVolume(String volumeId, String instanceId) {
        Instance instance = getInstance(instanceId);
        if (instance == null) {
            LOG.error("\nCan't get access to " + instanceId + " instance");
            System.exit(-1);
        }
        LOG.info("\ninst:" + instance);

        // create snapshot for AMI
        Volume volumeSrc = getVolume(volumeId);
        if (volumeSrc == null) {
            LOG.error("\nCan't get access to " + volumeId + " volume");
            System.exit(-1);
        }


        Snapshot snapshot = createSnapshot(volumeSrc);
        LOG.info("\nSnapshot created. Check snapshot data:\n" + snapshot.toString());

        // create volume
        String instanceAvailabilityZone = instance.getPlacement().getAvailabilityZone();
        Volume volumeDest = createVolumeFromSnapshot(snapshot, instanceAvailabilityZone);
        LOG.info("\nVolume created. Check volume data:\n" + volumeDest.toString());

        // mount AMI volume
        attachVolume(instance, volumeDest);
        return syncVolume(ec2client, volumeDest);
    }

    private Instance getInstance(String instanceId) {
        LinkedList<String> instanceIds = new LinkedList<>();
        instanceIds.add(instanceId);
        DescribeInstancesRequest describeInstancesRequest = new DescribeInstancesRequest();
        describeInstancesRequest.setInstanceIds(instanceIds);
        DescribeInstancesResult describeInstancesResult = ec2client.describeInstances(describeInstancesRequest);
        List<Reservation> reservations = describeInstancesResult.getReservations();
        List<Instance> insts = new LinkedList<>();
        for (Reservation r : reservations) {
            insts.addAll(r.getInstances());
        }

        if (insts.size() > 0) {
            return insts.get(0);
        }

        return null;
    }

    private Volume getVolume(String volumeId) {
        LinkedList<String> volumeIds = new LinkedList<>();
        volumeIds.add(volumeId);
        DescribeVolumesRequest describeVolumesRequest = new DescribeVolumesRequest(volumeIds);
        DescribeVolumesResult describeVolumesResult = ec2client.describeVolumes(describeVolumesRequest);
        if (describeVolumesResult.getVolumes().size() > 0) {
            return describeVolumesResult.getVolumes().get(0);
        }
        return null;
    }

    private Snapshot createSnapshot(Volume volume) {
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy.MM.dd'_T'hh:mm:ss");

        String volumeId = volume.getVolumeId();
        LOG.info(format("Starting creating snapshot for %s", volumeId));
        CreateSnapshotRequest snapshotRequest = new CreateSnapshotRequest(volumeId, volumeId + "__"
                + formatter.format(new Date(System.currentTimeMillis())));
        CreateSnapshotResult crSnapshotResult = ec2client.createSnapshot(snapshotRequest);
        Snapshot snapshot = crSnapshotResult.getSnapshot();
        return snapshot;
    }

    private Volume createVolumeFromSnapshot(Snapshot sourceSnapshot, String availabilityZoneName) {
        DescribeAvailabilityZonesResult zonesResult = ec2client.describeAvailabilityZones();
        List<AvailabilityZone> zones = zonesResult.getAvailabilityZones();
        Volume vol = null;
        if (zones.size() > 0) {
            LOG.info(format("Starting creating volume from %s", sourceSnapshot.getSnapshotId()));

            boolean incorrectState = true;
            long timeout = 10L;
            while (incorrectState) {
                try {
                    incorrectState = false;
                    CreateVolumeRequest crVolumeRequest = new CreateVolumeRequest(sourceSnapshot.getSnapshotId(),
                            availabilityZoneName);
                    CreateVolumeResult crVolumeResult = ec2client.createVolume(crVolumeRequest);
                    vol = crVolumeResult.getVolume();
                } catch (AmazonServiceException incorrectStateException) {
                    LOG.info(incorrectStateException.getMessage() + "\n Waiting for new try");
                    incorrectState = true;
                    timeout += timeout < 120 ? timeout * 2 : 0;
                    try {
                        TimeUnit.SECONDS.sleep(timeout);
                    } catch (InterruptedException e) {
                    }
                }
            }

        }
        return vol;
    }

    private void attachVolume(Instance instance, Volume volume) {
        String deviceName = getNextAvailableDeviceName(instance);
        boolean incorrectState = true;
        long timeout = 10L;
        while (incorrectState) {
            try {
                incorrectState = false;
                AttachVolumeRequest attachVolumeRequest = new AttachVolumeRequest(volume.getVolumeId(),
                        instance.getInstanceId(), deviceName);
                AttachVolumeResult res = ec2client.attachVolume(attachVolumeRequest);
            } catch (AmazonServiceException incorrectStateException) {
                LOG.info(incorrectStateException.getMessage() + "\n Waiting for new try");
                incorrectState = true;
                timeout += timeout < 120 ? timeout * 2 : 0;
                try {
                    TimeUnit.SECONDS.sleep(timeout);
                } catch (InterruptedException e) {
                    LOG.error(3);
                }
            }
        }
        LOG.info(format("\nVolume attached. check instance data\n %s", instance.toString()));
    }

    private String getNextAvailableDeviceName(Instance instance) {
        String devName = "";

        List<InstanceBlockDeviceMapping> devList = instance.getBlockDeviceMappings();
        for (InstanceBlockDeviceMapping map : devList) {
            String tmp = map.getDeviceName();
            if (tmp.compareToIgnoreCase(devName) > 0) {
                devName = tmp;
            }
        }

        if (devName.length() > 0) {
            char ch = devName.charAt(devName.length() - 1);
            if (ch < 'f') {
                ch = 'f' - 1;
            }
            if (ch < 'p') {
                ch += 1;
                return "/dev/sd" + (char) ch;
            }
        }
        return "/dev/sdf";
    }

    private Volume syncVolume(AmazonEC2 ec2client, Volume volume) {
        DescribeVolumesRequest describeVolumesRequest = new DescribeVolumesRequest();
        LinkedList<String> ids = new LinkedList<>();
        ids.add(volume.getVolumeId());
        describeVolumesRequest.setVolumeIds(ids);
        DescribeVolumesResult describeVolumesResult = ec2client.describeVolumes(describeVolumesRequest);
        return describeVolumesResult.getVolumes().get(0);
    }
}

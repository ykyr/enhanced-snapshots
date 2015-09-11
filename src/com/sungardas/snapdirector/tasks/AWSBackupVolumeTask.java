package com.sungardas.snapdirector.tasks;

import com.amazonaws.AmazonClientException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.model.Instance;
import com.amazonaws.services.ec2.model.Snapshot;
import com.amazonaws.services.ec2.model.Volume;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupState;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.aws.dynamodb.repository.BackupRepository;
import com.sungardas.snapdirector.aws.dynamodb.repository.TaskRepository;
import com.sungardas.snapdirector.service.AWSCommunticationService;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.RetentionService;
import com.sungardas.snapdirector.service.StorageService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.IOException;
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
    private AWSCredentials amazonAWSCredentials;

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

    @Autowired
    private RetentionService retentionService;

    private WorkerConfiguration configuration;

    public void setTaskEntry(TaskEntry taskEntry) {
        this.taskEntry = taskEntry;
    }

    public void execute() {
        String volumeId = taskEntry.getVolume();
        try {
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
                e1.printStackTrace();
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

            boolean backupStatus = false;
            try {
                String source = attachedDeviceName;
                LOG.info("Starting copying: " + source + " to:" + backupfileName);
                storageService.javaBinaryCopy(source, configuration.getSdfsMountPoint() + backupfileName);

                backupStatus = true;
            } catch (IOException | InterruptedException e) {
                LOG.fatal(format("Backup of volume %s failed", volumeId));
                LOG.fatal(e);
                File brocken = new File(configuration.getSdfsMountPoint() + backupfileName);
                if(brocken.exists()) {

                    if(brocken.delete()) {
                        LOG.info("Broken backup {} was deleted", brocken.getName());
                    }else {
                        LOG.info("Can't delete broken file {}", brocken.getName());
                    }
                }

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

            LOG.info(format("Backup process for volume %s finished successfully ", volumeId));
            LOG.info("Task " + taskEntry.getId() + ": Delete completed task:" + taskEntry.getId());
            taskRepository.delete(taskEntry);
            LOG.info("Task completed.");
            retentionService.apply();
        } catch (AmazonClientException e){
            LOG.info(format("Backup process for volume %s failed ", volumeId));
            taskRepository.delete(taskEntry);
        }
    }

    private Volume createAndAttachBackupVolume(String volumeId, String instanceId) {
        Instance instance = awsCommunication.getInstance(instanceId);
        if (instance == null) {
            LOG.error("\nCan't get access to " + instanceId + " instance");

        }
        LOG.info("\ninst:" + instance);

        // create snapshot for AMI
        Volume volumeSrc = awsCommunication.getVolume(volumeId);
        if (volumeSrc == null) {
            LOG.error("\nCan't get access to " + volumeId + " volume");

        }


        Snapshot snapshot = awsCommunication.waitForCompleteState(awsCommunication.createSnapshot(volumeSrc));
        LOG.info("\nSnapshot created. Check snapshot data:\n" + snapshot.toString());

        // create volume
        String instanceAvailabilityZone = instance.getPlacement().getAvailabilityZone();
        Volume volumeDest = awsCommunication.waitForAvailableState(awsCommunication.createVolumeFromSnapshot(snapshot, instanceAvailabilityZone));
        LOG.info("\nVolume created. Check volume data:\n" + volumeDest.toString());

        // mount AMI volume
        awsCommunication.attachVolume(instance, volumeDest);
        return awsCommunication.syncVolume(volumeDest);
    }
}

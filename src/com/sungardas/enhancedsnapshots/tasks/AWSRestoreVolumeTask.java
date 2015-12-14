package com.sungardas.enhancedsnapshots.tasks;

import com.amazonaws.services.ec2.model.Instance;
import com.amazonaws.services.ec2.model.Snapshot;
import com.amazonaws.services.ec2.model.Volume;
import com.amazonaws.services.ec2.model.VolumeType;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.BackupEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.TaskEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.BackupRepository;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.TaskRepository;
import com.sungardas.enhancedsnapshots.dto.CopyingTaskProgressDto;
import com.sungardas.enhancedsnapshots.exception.DataAccessException;
import com.sungardas.enhancedsnapshots.exception.EnhancedSnapshotsInterruptedException;
import com.sungardas.enhancedsnapshots.service.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import static java.lang.String.format;

@Component
@Scope("prototype")
@Profile("prod")
public class AWSRestoreVolumeTask implements RestoreTask {
    public static final String RESTORED_NAME_PREFIX = "Restore of ";
    private static final Logger LOG = LogManager.getLogger(AWSRestoreVolumeTask.class);
    @Autowired
    private TaskRepository taskRepository;
    @Autowired
    private BackupRepository backupRepository;

	@Value("${sungardas.worker.configuration}")
	private String configurationId;
	
	@Autowired
	private SnapshotService snapshotService;

	@Autowired
	private AWSCommunicationService awsCommunication;

    @Autowired
    private StorageService storageService;

    @Autowired
    private ConfigurationService configurationService;

    @Autowired
    private NotificationService notificationService;

    @Autowired
    private TaskService taskService;

    private WorkerConfiguration configuration;

    private TaskEntry taskEntry;

    @Override
    public void setTaskEntry(TaskEntry taskEntry) {
        this.taskEntry = taskEntry;

    }

    @Override
    public void execute() {
        LOG.info("Executing restore task:\n" + taskEntry.toString());
        if (Thread.interrupted()) {
            throw new EnhancedSnapshotsInterruptedException("Task interrupted");
        }
        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Starting restore", 0);
        String sourceFile = taskEntry.getSourceFileName();
        configuration = configurationService.getWorkerConfiguration();
        changeTaskStatusToRunning();
        try {
            if (sourceFile == null || sourceFile.isEmpty()) {
                LOG.info("Task was defined as restore from snapshot.");
                restoreFromSnapshot();
            } else {
                LOG.info("Task was defined as restore from history.");
                restoreFromBackupFile();
            }
            completeTask();
        } catch (RuntimeException e) {
            e.printStackTrace();
            LOG.error("Failed to execute {} task {}. Changing task status to '{}'", taskEntry.getType(), taskEntry.getId(), TaskEntry.TaskEntryStatus.ERROR);
            taskEntry.setStatus(TaskEntry.TaskEntryStatus.ERROR.getStatus());
            taskRepository.save(taskEntry);
        }
    }

    private void changeTaskStatusToRunning() {
        LOG.info("Status of {} task {} was changed to '{}'", taskEntry.getType(), taskEntry.getId(), TaskEntry.TaskEntryStatus.RUNNING);
        taskEntry.setStatus(TaskEntry.TaskEntryStatus.RUNNING.getStatus());
        taskRepository.save(taskEntry);
    }

    private void completeTask() {
        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Restore complete", 100);
        taskService.complete(taskEntry);
        LOG.info("{} task {} was completed", taskEntry.getType(), taskEntry.getId());
    }

	private void restoreFromSnapshot() {
        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Restore from snapshot", 20);
        String targetZone = taskEntry.getAvailabilityZone();

		String volumeId = taskEntry.getVolume();
		String snapshotId = snapshotService.getSnapshotId(volumeId, configurationId);
		BackupEntry backupEntry = backupRepository.getLast(volumeId, configurationId);
		if (snapshotId == null) {
			LOG.error("Failed to find snapshot for volume {} ", volumeId);
			throw new DataAccessException("Backup for volume: " + volumeId + " was not found");
		}
        if (Thread.interrupted()) {
            throw new EnhancedSnapshotsInterruptedException("Task interrupted");
        }
        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Creating volume from snapshot", 50);
        Volume volume = awsCommunication.createVolumeFromSnapshot(snapshotId, targetZone);
		awsCommunication.setResourceName(volume.getVolumeId(), RESTORED_NAME_PREFIX + backupEntry.getVolumeId());
        awsCommunication.addTag(volume.getVolumeId(), "Created by", "Enhanced Snapshots");
    }

    private void restoreFromBackupFile() {
        if (Thread.interrupted()) {
            throw new EnhancedSnapshotsInterruptedException("Task interrupted");
        }
        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Restore from file", 10);
        String targetZone = taskEntry.getAvailabilityZone();
        String sourceFile =taskEntry.getSourceFileName();
        String instanceId = taskEntry.getInstanceId();

        BackupEntry backupentry = backupRepository.getByBackupFileName(sourceFile);
        LOG.info("Used backup record:\n" + backupentry.toString());
        Instance instance = awsCommunication.getInstance(instanceId);
        String volumeType = backupentry.getVolumeType();
        String size = backupentry.getSizeGiB();
        String iops = backupentry.getIops();
        if (Thread.interrupted()) {
            throw new EnhancedSnapshotsInterruptedException("Task interrupted");
        }
        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Creating volume...", 15);
        Volume tempVolume = null;
        switch (VolumeType.fromValue(volumeType)) {
            case Standard:
                tempVolume = awsCommunication.createStandardVolume(Integer.parseInt(size));
                LOG.info("Created standard volume:\n" + tempVolume.toString());
                break;
            case Gp2:
                tempVolume = awsCommunication.createGP2Volume(Integer.parseInt(size));
                LOG.info("Created GP2 volume:\n" + tempVolume.toString());
                break;
            case Io1:
                tempVolume = awsCommunication.createIO1Volume(Integer.parseInt(size), Integer.parseInt(iops));
                LOG.info("Created IO1 volume:\n" + tempVolume.toString());
                break;
        }
        if (Thread.interrupted()) {
            throw new EnhancedSnapshotsInterruptedException("Task interrupted");
        }
        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Attaching volume...", 20);
        awsCommunication.createTemporaryTag(tempVolume.getVolumeId(), backupentry.getFileName());
        awsCommunication.attachVolume(instance, tempVolume);
        try {
            TimeUnit.MINUTES.sleep(1);
        } catch (InterruptedException e1) {
            e1.printStackTrace();
        }
        LOG.info("Trying to attach volume to innstance " + instance.getInstanceId());
        //wait for attached state

        if (Thread.interrupted()) {
            throw new EnhancedSnapshotsInterruptedException("Task interrupted");
        }
        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Synchronizing volume...", 25);
        while (tempVolume.getAttachments().size() == 0) {
            sleep();
            tempVolume = awsCommunication.syncVolume(tempVolume);
        }

        String attachedDeviceName = storageService.detectFsDevName(tempVolume);
        LOG.info("Volume was attached as device: " + attachedDeviceName);
        try {
            CopyingTaskProgressDto dto = new CopyingTaskProgressDto(taskEntry.getId(), 25, 80, Long.parseLong(backupentry.getSizeGiB()));
            storageService.javaBinaryCopy(configuration.getSdfsMountPoint() + backupentry.getFileName(), attachedDeviceName, dto);
        } catch (IOException | InterruptedException e) {
            LOG.fatal(format("Restore of volume %s failed", tempVolume));
            taskEntry.setStatus("error");
            e.printStackTrace();
        }

        if (Thread.interrupted()) {
            throw new EnhancedSnapshotsInterruptedException("Task interrupted");
        }
        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Detaching volume...", 85);
        awsCommunication.detachVolume(tempVolume);
        LOG.info("Detaching volume after restoring data: " + tempVolume.toString());

        if (Thread.interrupted()) {
            throw new EnhancedSnapshotsInterruptedException("Task interrupted");
        }
        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Moving into target zone...", 90);
        Snapshot tempSnapshot = awsCommunication.createSnapshot(tempVolume);
        if (Thread.interrupted()) {
            throw new EnhancedSnapshotsInterruptedException("Task interrupted");
        }
        notificationService.notifyAboutTaskProgress(taskEntry.getId(), "Moving into target zone...", 95);
        Volume volumeToRestore = awsCommunication.createVolumeFromSnapshot(tempSnapshot.getSnapshotId(), targetZone);

        if (Thread.interrupted()) {
            throw new EnhancedSnapshotsInterruptedException("Task interrupted");
        }
        awsCommunication.setResourceName(volumeToRestore.getVolumeId(), RESTORED_NAME_PREFIX + backupentry.getFileName());
        awsCommunication.deleteVolume(tempVolume);
        awsCommunication.addTag(volumeToRestore.getVolumeId(), "Created by", "Enhanced Snapshots");
    }

    private void sleep() {
        try {
            TimeUnit.SECONDS.sleep(10);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}

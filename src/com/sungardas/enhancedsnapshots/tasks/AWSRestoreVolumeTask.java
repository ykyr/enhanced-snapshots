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
import com.sungardas.enhancedsnapshots.service.AWSCommunicationService;
import com.sungardas.enhancedsnapshots.service.ConfigurationService;
import com.sungardas.enhancedsnapshots.service.SnapshotService;
import com.sungardas.enhancedsnapshots.service.StorageService;
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

    private TaskEntry taskEntry;

    @Autowired
    private ConfigurationService configurationService;

    private WorkerConfiguration configuration;

    @Override
    public void setTaskEntry(TaskEntry taskEntry) {
        this.taskEntry = taskEntry;

    }

    @Override
    public void execute() {
        LOG.info("Executing restore task:\n" + taskEntry.toString());
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
            deleteCompletedTask();
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

    private void deleteCompletedTask() {
        LOG.info("Deleting completed {} task {}", taskEntry.getType(), taskEntry.getId());
        taskRepository.delete(taskEntry);
        LOG.info("{} task {} was completed and removed", taskEntry.getType(), taskEntry.getId());
    }

	private void restoreFromSnapshot() {
        String targetZone = taskEntry.getAvailabilityZone();

		String volumeId = taskEntry.getVolume();
		String snapshotId = snapshotService.getSnapshotId(volumeId, configurationId);
		BackupEntry backupEntry = backupRepository.getLast(volumeId, configurationId);
		if (snapshotId == null) {
			LOG.error("Failed to find snapshot for volume {} ", volumeId);
			throw new DataAccessException("Backup for volume: " + volumeId + " was not found");
		}
		Volume volume = awsCommunication.createVolumeFromSnapshot(snapshotId, targetZone);
		awsCommunication.setResourceName(volume.getVolumeId(), RESTORED_NAME_PREFIX + backupEntry.getVolumeId());
	}

    private void restoreFromBackupFile() {
        String targetZone = taskEntry.getAvailabilityZone();
        String sourceFile =taskEntry.getSourceFileName();
        String instanceId = taskEntry.getInstanceId();

        BackupEntry backupentry = backupRepository.getByBackupFileName(sourceFile);
        LOG.info("Used backup record:\n" + backupentry.toString());
        Instance instance = awsCommunication.getInstance(instanceId);
        String volumeType = backupentry.getVolumeType();
        String size = backupentry.getSizeGiB();
        String iops = backupentry.getIops();
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
        awsCommunication.createTemporaryTag(tempVolume.getVolumeId(),backupentry.getFileName());
        awsCommunication.attachVolume(instance, tempVolume);
        try {
            TimeUnit.MINUTES.sleep(1);
        } catch (InterruptedException e1) {
            e1.printStackTrace();
        }
        LOG.info("Trying to attach volume to innstance " + instance.getInstanceId());
        //wait for attached state

        while (tempVolume.getAttachments().size() == 0) {
            sleep();
            tempVolume = awsCommunication.syncVolume(tempVolume);
        }

        String attachedDeviceName = storageService.detectFsDevName(tempVolume);
        LOG.info("Volume was attached as device: " + attachedDeviceName);
        try {
            CopyingTaskProgressDto dto = new CopyingTaskProgressDto(taskEntry.getId(), 15, 80);
            storageService.javaBinaryCopy(configuration.getSdfsMountPoint() + backupentry.getFileName(), attachedDeviceName, dto);
        } catch (IOException | InterruptedException e) {
            LOG.fatal(format("Restore of volume %s failed", tempVolume));
            taskEntry.setStatus("error");
            e.printStackTrace();
        }

        awsCommunication.detachVolume(tempVolume);
        LOG.info("Detaching volume after restoring data: " + tempVolume.toString());

        Snapshot tempSnapshot = awsCommunication.createSnapshot(tempVolume);
        Volume volumeToRestore = awsCommunication.createVolumeFromSnapshot(tempSnapshot.getSnapshotId(), targetZone);

        awsCommunication.setResourceName(volumeToRestore.getVolumeId(), RESTORED_NAME_PREFIX + backupentry.getFileName());
        awsCommunication.deleteVolume(tempVolume);
    }

    private void sleep() {
        try {
            TimeUnit.SECONDS.sleep(10);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}

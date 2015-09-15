package com.sungardas.snapdirector.tasks;

import com.amazonaws.services.ec2.model.Instance;
import com.amazonaws.services.ec2.model.Volume;
import com.amazonaws.services.ec2.model.VolumeType;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.aws.dynamodb.repository.BackupRepository;
import com.sungardas.snapdirector.aws.dynamodb.repository.TaskRepository;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.service.AWSCommunticationService;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.SnapshotService;
import com.sungardas.snapdirector.service.StorageService;
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
	private AWSCommunticationService awsCommunication;

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
        String sourceFile = taskEntry.getOptions();
        configuration = configurationService.getConfiguration();
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
		String volumeId = taskEntry.getVolume();
		String snapshotId = snapshotService.getSnapshotId(volumeId, configurationId);
		BackupEntry backupEntry = backupRepository.getLast(volumeId);
		if (snapshotId == null) {
			LOG.error("Failed to find snapshot for volume {} ", volumeId);
			throw new DataAccessException("Backup for volume: " + volumeId + " was not found");
		}
		Volume volume = awsCommunication.createVolumeFromSnapshot(snapshotId, awsCommunication.getVolume(volumeId).getAvailabilityZone());
		awsCommunication.setResourceName(volume.getVolumeId(), "Restore of "+backupEntry.getVolumeId());
	}

    private void restoreFromBackupFile() {
        String sourceFile = taskEntry.getOptions();
        String instanceId = taskEntry.getInstanceId();

        BackupEntry backupentry = backupRepository.getByBackupFileName(sourceFile);
        LOG.info("Used backup record:\n" + backupentry.toString());
        Instance instance = awsCommunication.getInstance(instanceId);
        String volumeType = backupentry.getVolumeType();
        String size = backupentry.getSizeGiB();
        String iops = backupentry.getIops();
        Volume volumeToRestore = null;
        switch (VolumeType.fromValue(volumeType)) {
            case Standard:
                volumeToRestore = awsCommunication.createStandardVolume(Integer.parseInt(size));
                LOG.info("Created standard volume:\n" + volumeToRestore.toString());
                break;
            case Gp2:
                volumeToRestore = awsCommunication.createGP2Volume(Integer.parseInt(size));
                LOG.info("Created GP2 volume:\n" + volumeToRestore.toString());
                break;
            case Io1:
                volumeToRestore = awsCommunication.createIO1Volume(Integer.parseInt(size), Integer.parseInt(iops));
                LOG.info("Created IO1 volume:\n" + volumeToRestore.toString());
                break;
        }

        awsCommunication.attachVolume(instance, volumeToRestore);
        try {
            TimeUnit.MINUTES.sleep(1);
        } catch (InterruptedException e1) {
            e1.printStackTrace();
        }
        LOG.info("Trying to attach volume to innstance " + instance.getInstanceId());
        //wait for attached state

        while (volumeToRestore.getAttachments().size() == 0) {
            sleep();
            volumeToRestore = awsCommunication.syncVolume(volumeToRestore);
        }

        String attachedDeviceName = storageService.detectFsDevName(volumeToRestore);
        LOG.info("Volume was attached as device: " + attachedDeviceName);
        try {
            storageService.javaBinaryCopy(configuration.getSdfsMountPoint() + backupentry.getFileName(), attachedDeviceName);
        } catch (IOException | InterruptedException e) {
            LOG.fatal(format("Restore of volume %s failed", volumeToRestore));
            taskEntry.setStatus("error");
            e.printStackTrace();
        }

        awsCommunication.detachVolume(volumeToRestore);
        LOG.info("Detaching volume after restoring data: " + volumeToRestore.toString());
        awsCommunication.setResourceName(volumeToRestore.getVolumeId(), backupentry.getFileName());
    }

    private void sleep() {
        try {
            TimeUnit.SECONDS.sleep(10);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}

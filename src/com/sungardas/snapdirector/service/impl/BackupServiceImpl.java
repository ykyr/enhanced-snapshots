package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.TaskRepository;
import com.sungardas.snapdirector.service.BackupService;
import com.sungardas.snapdirector.service.ConfigurationService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry.TaskEntryType.DELETE;

@Service
public class BackupServiceImpl implements BackupService {

    private static final Logger LOG = LogManager.getLogger(BackupServiceImpl.class);

    private static final String BACKUP_FILE_EXT = ".backup";

    @Autowired
    private ConfigurationService configurationService;

    @Autowired
    private TaskRepository taskRepository;

    @Override
    public void deleteBackup(String backupName, String user) {
        TaskEntry taskEntry = getDeleteTask(backupName, user);
        taskRepository.save(taskEntry);
    }

    @Override
    public void deleteBackup(Collection<BackupEntry> backupEntries, String user) {
        LOG.debug("Removing backups: {}", backupEntries);
        List<TaskEntry> tasks = new ArrayList<>();

        for (BackupEntry entry : backupEntries) {
            tasks.add(getDeleteTask(entry.getFileName(), user));
        }

        taskRepository.save(tasks);
    }

    private String getVolumeId(String backupName){
        return backupName.substring(0, 12);
    }

    private TaskEntry getDeleteTask(String backupName, String user){
        String volumeId = getVolumeId(backupName);

        TaskEntry taskEntry = new TaskEntry();

        taskEntry.setVolume(volumeId);
        taskEntry.setType(DELETE.getType());
        taskEntry.setInstanceId(configurationService.getConfiguration().getConfigurationId());
        taskEntry.setStatus(TaskEntry.TaskEntryStatus.WAITING.getStatus());
        taskEntry.setOptions(backupName + BACKUP_FILE_EXT);
        taskEntry.setSchedulerName(user);
        taskEntry.setSchedulerTime(String.valueOf(DateTime.now().getMillis()));

        //TODO Remove hardcode
        taskEntry.setWorker(taskEntry.getInstanceId());
        taskEntry.setPriority(0);
        taskEntry.setSchedulerManual(true);

        return taskEntry;
    }
}

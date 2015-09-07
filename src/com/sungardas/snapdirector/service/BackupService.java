package com.sungardas.snapdirector.service;

import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;

import java.util.List;

public interface BackupService {
    void deleteBackup(String backupName, String user);

    List<BackupEntry> getBackupList(String volumeId);
}

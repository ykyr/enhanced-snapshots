package com.sungardas.snapdirector.service;

import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;

import java.util.List;

import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;

import java.util.Collection;

public interface BackupService {
    void deleteBackup(String backupName, String user);

    List<BackupEntry> getBackupList(String volumeId);

    void deleteBackup(Collection<BackupEntry> backupEntries, String user);

    void deleteAllBackups();
}

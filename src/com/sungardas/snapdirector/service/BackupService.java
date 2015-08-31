package com.sungardas.snapdirector.service;

import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;

import java.util.Collection;

public interface BackupService {
    void deleteBackup(String backupName, String user);

    void deleteBackup(Collection<BackupEntry> backupEntries, String user);
}

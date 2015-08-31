package com.sungardas.snapdirector.aws.dynamodb.repository;

import java.util.List;

import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;

public interface BackupRepository {

	void save(BackupEntry backup);

    void delete(BackupEntry backupEntry);

	List<BackupEntry> get(String volumeId);

	BackupEntry getLast(String volumeId);

	BackupEntry getByBackupFileName(String backupName);

    List<BackupEntry> findAll();
}

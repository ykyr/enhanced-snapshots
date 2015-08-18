package com.sungardas.snapdirector.aws.dynamodb.repository;

import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;

public interface BackupRepository {

	void save(BackupEntry backup);

}

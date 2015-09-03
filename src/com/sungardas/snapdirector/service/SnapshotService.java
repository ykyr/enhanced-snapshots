package com.sungardas.snapdirector.service;

import com.sungardas.snapdirector.dto.SnapshotDto;

public interface SnapshotService {
	void addSnapshot(SnapshotDto newSnapshot);
	void removeAllSnapshotsExceptOne(String snapshotToLeave);
	void removeSnapshot(String snapshotId);
}

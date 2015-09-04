package com.sungardas.snapdirector.service;

import java.util.List;

import com.sungardas.snapdirector.dto.SnapshotDto;

public interface SnapshotService {
	
	void addSnapshot(SnapshotDto newSnapshot);
	
	void removeSnapshot(String snapshotId);
	
	List<SnapshotDto> getSnapshotsToDelete(String volumeId, String snapshotToLeave);
}

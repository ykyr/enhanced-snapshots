package com.sungardas.snapdirector.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.sungardas.snapdirector.aws.dynamodb.repository.SnapshotRepository;
import com.sungardas.snapdirector.dto.SnapshotDto;
import com.sungardas.snapdirector.dto.converter.SnapshotDtoConverter;
import com.sungardas.snapdirector.service.SnapshotService;

@Service
public class SnapshotServiceImpl implements SnapshotService {

	@Autowired
	private SnapshotRepository snapshotRepository;
	
	@Override
	public void addSnapshot(SnapshotDto newSnapshot) {
		if(newSnapshot == null){
			throw new IllegalArgumentException("Provided argument is null");
		}
		
		snapshotRepository.save(SnapshotDtoConverter.convert(newSnapshot));

	}

	@Override
	public void removeAllSnapshotsExceptOne(String snapshotToLeave) {
		if (snapshotToLeave == null || snapshotToLeave.length() != 13){
			throw new IllegalArgumentException("Incorrect SnapshotID");
		}
		
		snapshotRepository.deleteBySnapshotIdNot(snapshotToLeave);

	}

	@Override
	public void removeSnapshot(String snapshotId) {
		if (snapshotId == null || snapshotId.length() != 13){
			throw new IllegalArgumentException("Incorrect SnapshotID");
		}
		
		snapshotRepository.deleteBySnapshotId(snapshotId);

	}

}

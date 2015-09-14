package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.aws.dynamodb.model.Snapshot;
import com.sungardas.snapdirector.aws.dynamodb.repository.SnapshotRepository;
import com.sungardas.snapdirector.service.SnapshotService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.stereotype.Service;

@Service
public class SnapshotServiceImpl implements SnapshotService {

    @Autowired
    private SnapshotRepository snapshotRepository;

    @Override
    public void addSnapshot(String snapshotId, String volumeId) {
        if (snapshotId == null && volumeId != null) {
            throw new IllegalArgumentException("Provided argument is null");
        }
        snapshotRepository.save(new Snapshot(snapshotId, volumeId));
    }

    @Override
    public void removeSnapshot(String snapshotId) {
        if (snapshotId == null) {
            throw new IllegalArgumentException("Incorrect SnapshotID");
        }
        try {
            snapshotRepository.delete(snapshotId);
        } catch (EmptyResultDataAccessException e) {
            // Snapshot not found
        }
    }
}

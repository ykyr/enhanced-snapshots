package com.sungardas.snapdirector.service.impl;

import com.amazonaws.AmazonClientException;
import com.sungardas.snapdirector.aws.dynamodb.model.SnapshotEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.SnapshotRepository;
import com.sungardas.snapdirector.service.SnapshotService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class SnapshotServiceImpl implements SnapshotService {

    @Autowired
    private SnapshotRepository snapshotRepository;

    @Override
    public String getSnapshotId(String volumeId, String instancreId) {
        List<SnapshotEntry> entries;
        entries = snapshotRepository.findByVolumeIdAndInstanceId(volumeId, instancreId);
//        SnapshotEntry entry =  snapshotRepository.getByVolumeIdAndSnapshotId(
//               new SnapshotEntry(volumeId, instancreId));
        if(entries!=null && entries.size()>0) {
            return entries.get(0).getSnapshotId();
        } else return null;

    }

    @Override
    public void saveSnapshot(String volumeId, String instanceId, String snapshotId) {
            snapshotRepository.save(new SnapshotEntry(volumeId, instanceId, snapshotId));
    }





}

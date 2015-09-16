package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.aws.dynamodb.model.SnapshotEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.SnapshotRepository;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.SnapshotService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.List;

@Service
public class SnapshotServiceImpl implements SnapshotService {

    @Autowired
    private SnapshotRepository snapshotRepository;

    private String instanceId;

    @Autowired
    private ConfigurationService configurationService;

    @PostConstruct
    private void init() {
        instanceId = configurationService.getWorkerConfiguration().getConfigurationId();
    }

    @Override
    public String getSnapshotId(String volumeId, String instancreId) {
        List<SnapshotEntry> entries;
        entries = snapshotRepository.findByVolumeInstanceId(volumeId + ":" + instancreId);
        if (entries != null && entries.size() > 0) {
            return entries.get(0).getSnapshotId();
        } else return null;

    }

    @Override
    public void saveSnapshot(String volumeId, String instanceId, String snapshotId) {
        snapshotRepository.save(new SnapshotEntry(instanceId, snapshotId, volumeId));
    }

    @Override
    public void deleteAllSnapshots() {
        List<SnapshotEntry> snapshotList = snapshotRepository.findByInstanceId(instanceId);
        snapshotRepository.delete(snapshotList);
    }

    @Override
    public boolean isTableEmpty() {
        return snapshotRepository.count() == 0;
    }
}

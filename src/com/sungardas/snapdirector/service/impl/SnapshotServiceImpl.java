package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.aws.dynamodb.model.SnapshotEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.SnapshotRepository;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.SnapshotService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.List;

@Service
public class SnapshotServiceImpl implements SnapshotService {
    private static final Logger LOG = LogManager.getLogger(SnapshotServiceImpl.class);

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
        LOG.info("Get snapshot id for volume {} and configuration {}", volumeId, instancreId);
        try {
            return snapshotRepository.findOne(SnapshotEntry.getId(volumeId, instancreId)).getSnapshotId();
        } catch (Exception e) {
            return null;
        }
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

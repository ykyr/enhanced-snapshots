package com.sungardas.enhancedsnapshots.service.impl;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.SnapshotEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.SnapshotRepository;
import com.sungardas.enhancedsnapshots.service.ConfigurationService;
import com.sungardas.enhancedsnapshots.service.SnapshotService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;

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
        instanceId = configurationService.getConfiguration().getConfigurationId();
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
}

package com.sungardas.enhancedsnapshots.service.impl;

import com.amazonaws.services.s3.AmazonS3;
import com.sungardas.enhancedsnapshots.service.ConfigurationService;
import com.sungardas.enhancedsnapshots.service.SDFSStateService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import javax.annotation.PostConstruct;
import java.io.File;


class CreateAppConfigurationImpl {
    private static final Logger LOG = LogManager.getLogger(CreateAppConfigurationImpl.class);

    @Autowired
    private SDFSStateService sdfsService;

    @Autowired
    private ConfigurationService configurationService;

    @Autowired
    private AmazonS3 amazonS3;

    private boolean init = false;

    @PostConstruct
    private void init() {
        if (!init) {
            LOG.info("Initialization started");
            init = true;
            boolean isBucketContainsSDFSMetadata = false;
            if (isBucketExits(configurationService.getS3Bucket())) {
                isBucketContainsSDFSMetadata = sdfsService.containsSdfsMetadata(configurationService.getS3Bucket());
            }
            LOG.info("Initialization SDFS");
            if (isBucketContainsSDFSMetadata) {
                LOG.info("Restoring SDFS from backup");
                sdfsService.restoreSDFS();
            } else {
                File sdfsConfig = new File(configurationService.getSdfsConfigPath());
                LOG.info("SDFS config: {}", sdfsConfig.getPath());
                if (sdfsConfig.exists()) {
                    sdfsConfig.delete();
                }
                LOG.info("Starting SDFS");
                sdfsService.startSDFS();
            }
            LOG.info("Initialization finished");
        }
    }

    private boolean isBucketExits(String s3Bucket) {
        try {
            String location = amazonS3.getBucketLocation(s3Bucket);
            return location != null;
        } catch (Exception e) {
            return false;
        }
    }
}

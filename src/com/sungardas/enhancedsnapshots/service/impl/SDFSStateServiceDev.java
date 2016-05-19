package com.sungardas.enhancedsnapshots.service.impl;
import com.amazonaws.AmazonClientException;
import com.sungardas.enhancedsnapshots.service.SDFSStateService;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile("dev")
public class SDFSStateServiceDev implements SDFSStateService {
    private static final org.apache.log4j.Logger LOG = org.apache.log4j.LogManager.getLogger(SDFSStateServiceDev.class);

    @Override
    public void backupState(String taskId) {

    }

    @Override
    public void restoreState() throws AmazonClientException {
    }

    @Override
    public boolean containsSdfsMetadata(String sBucket) {
        return false;

    }

    @Override
    public Long getBackupTime() {
        return System.currentTimeMillis();
    }

    @Override
    public void shutdownSDFS(String size, String bucketName) {

    }

    @Override
    public void startupSDFS(String size, String bucketName,  Boolean isRestore) {

    }

}

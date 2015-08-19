package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.exception.SDFSException;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.StorageService;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

@Service
public class SDFSServiceImpl implements StorageService {

    public static final Log LOG = LogFactory.getLog(SDFSServiceImpl.class);

    private String sdfs;
    private String mountPoint;

    @Autowired
    private ConfigurationService configurationService;

    @PostConstruct
    public void init() {
        WorkerConfiguration configuration = configurationService.getConfiguration();
        this.sdfs = configuration.getSdfsVolumeName();
        this.mountPoint = configuration.getSdfsMountPoint();
    }

    @Override
    public void deleteFile(String fileName) {
        Path path = Paths.get(mountPoint, fileName);
        File file = path.toFile();
        if(file.exists()){
            file.delete();
        } else{
            LOG.error("File not found "+file.getAbsolutePath());
            throw new SDFSException("File not found "+file.getAbsolutePath());
        }
    }
}

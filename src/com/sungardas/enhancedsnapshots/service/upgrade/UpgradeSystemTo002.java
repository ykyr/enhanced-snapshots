package com.sungardas.enhancedsnapshots.service.upgrade;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import com.amazonaws.util.EC2MetadataUtils;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.BackupEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.BackupState;
import com.sungardas.enhancedsnapshots.service.SDFSStateService;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.codehaus.jackson.map.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class UpgradeSystemTo002 implements SystemUpgrade {

    private static final Logger LOG = LogManager.getLogger(UpgradeSystemTo002.class);
    @Value("${enhancedsnapshots.default.sdfs.mount.point}")
    private String mountPoint;
    private static final String upgradeVersion = "0.0.2";

    // do not move to property file
    private static final String sdfsSystemBackupArchive = "sdfsstate.zip";

    private final ObjectMapper objectMapper = new ObjectMapper();
    @Autowired
    private SDFSStateService sdfsStateService;


    private static final int VOLUME_ID_INDEX = 0;
    private static final int TIME_INDEX = 1;
    private static final int TYPE_INDEX = 2;
    private static final int IOPS_INDEX = 3;
    private static final long BYTES_IN_GIB = 1073741824l;


    @Override
    public void upgrade(Path tempFolder, String initVersion) {
        try {
            if (stringVersionToInt(initVersion) >= stringVersionToInt(upgradeVersion)) {
                LOG.info("No need to upgrade to {}", upgradeVersion);
                return;
            }
            LOG.info("Upgrading system to version {}", upgradeVersion);
            File destForBackups = Paths.get(tempFolder.toString(), BackupEntry.class.getName()).toFile();
            sdfsStateService.restoreSDFS(sdfsSystemBackupArchive);
            objectMapper.writeValue(destForBackups, restoreBackups());
        } catch (Exception e) {
            LOG.error("Failed to upgrade system: ", e);
        }
        finally {
            sdfsStateService.stopSDFS();
        }
    }


    protected int stringVersionToInt(String version){
       return Integer.parseInt(version.replace(".", ""));
    }

    private List<BackupEntry> restoreBackups() {
        File[] files = new File(mountPoint).listFiles();
        LOG.info("Found {} files in system backup", files.length);
        List<BackupEntry> backupEntryList = new ArrayList<>();
        for (File file : files) {
            BackupEntry entry = getBackupFromFile(file);
            if (entry != null) {
                backupEntryList.add(entry);
            }
        }
        LOG.info("All backups restored.");
        return backupEntryList;
    }

    private BackupEntry getBackupFromFile(File file) {
        String fileName = file.getName();
        String[] props = fileName.split("\\.");
        if (props.length != 5) {
            return null;
        } else {
            BackupEntry backupEntry = new BackupEntry();

            backupEntry.setFileName(fileName);
            backupEntry.setIops(props[IOPS_INDEX]);
            backupEntry.setSizeGiB(String.valueOf((int) (file.length() / BYTES_IN_GIB)));
            backupEntry.setTimeCreated(props[TIME_INDEX]);
            backupEntry.setVolumeType(props[TYPE_INDEX]);
            backupEntry.setState(BackupState.COMPLETED.getState());
            backupEntry.setVolumeId(props[VOLUME_ID_INDEX]);
            backupEntry.setSize(String.valueOf(file.length()));

            return backupEntry;
        }
    }

    protected String getInstanceId() {
        return EC2MetadataUtils.getInstanceId();
    }

}

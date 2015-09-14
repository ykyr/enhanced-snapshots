package com.sungardas.snapdirector.service.impl;

import com.amazonaws.AmazonClientException;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.RetentionEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.BackupRepository;
import com.sungardas.snapdirector.aws.dynamodb.repository.RetentionRepository;
import com.sungardas.snapdirector.dto.RetentionDto;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.service.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.*;

import static com.sungardas.snapdirector.dto.converter.RetentionConverter.toDto;
import static com.sungardas.snapdirector.dto.converter.RetentionConverter.toEntry;

@Service
public class RetentionServiceImpl implements RetentionService {

    public static final String RETENTION_USER = "RETENTION POLICY";

    private static final Logger LOG = LogManager.getLogger(RetentionServiceImpl.class);

    @Autowired
    private RetentionRepository retentionRepository;

    @Autowired
    private BackupRepository backupRepository;

    @Autowired
    private BackupService backupService;

    @Autowired
    private VolumeService volumeService;

    @Autowired
    private SchedulerService schedulerService;

    @Value("${snapdirector.retention.cron}")
    private String cronExpression;

    @PostConstruct
    private void init() {
        schedulerService.addTask(getJob(this), cronExpression);
        try {
            apply();
        } catch (AmazonClientException e){
            LOG.error(e);
        }
    }

    @Override
    public void putRetention(RetentionDto retentionDto) {
        if (volumeService.volumeExists(retentionDto.getVolumeId())) {
            RetentionEntry entry = toEntry(retentionDto);

            retentionRepository.save(entry);
            apply();
        } else {
            LOG.error("Volume with id: {} not found", retentionDto.getVolumeId());
            throw new DataAccessException("Volume with id:" + retentionDto.getVolumeId() + " not found");
        }
    }

    @Override
    public RetentionDto getRetentionDto(String volumeId) {
        RetentionEntry entry = retentionRepository.findOne(volumeId);

        if (entry != null) {
            return toDto(entry);
        } else {
            if (volumeService.volumeExists(volumeId)) {
                return new RetentionDto(volumeId);
            } else {
                LOG.error("Volume with id: {} not found", volumeId);
                throw new DataAccessException("Volume with id:" + volumeId + " not found");
            }
        }
    }

    @Override
    public void apply() {
        LOG.debug("Retention started");
        Map<String, Set<BackupEntry>> backups = getBackups();
        Map<String, RetentionEntry> retentions = getRetentions();
        Set<BackupEntry> backupsToRemove = new LinkedHashSet<>();
        Set<RetentionEntry> retentionsToRemove = new HashSet<>();

        for (Map.Entry<String, Set<BackupEntry>> entry : backups.entrySet()) {
            RetentionEntry retentionEntry = retentions.get(entry.getKey());
            if (retentionEntry != null) {
                if (isEmpty(retentionEntry)) {
                    retentionsToRemove.add(retentionEntry);
                } else {
                    BackupEntry[] values = entry.getValue().toArray(new BackupEntry[entry.getValue().size()]);
                    applySizeRetention(backupsToRemove, values, retentionEntry);
                    applyCountRetention(backupsToRemove, values, retentionEntry);
                    applyDayRetention(backupsToRemove, values, retentionEntry);
                }
            }
        }

        for (Map.Entry<String, RetentionEntry> entry : retentions.entrySet()) {
            if (!backups.containsKey(entry.getKey())) {
                retentionsToRemove.add(entry.getValue());
            }
        }

        if (!retentionsToRemove.isEmpty()) {
            LOG.debug("Found empty retentions: {}", retentionsToRemove);
            retentionRepository.delete(retentionsToRemove);
            LOG.debug("Empty retentions successfully removed");
        }

        if (!backupsToRemove.isEmpty()) {
            LOG.debug("Found backup to remove: {}", backupsToRemove);
            backupService.deleteBackup(backupsToRemove, RETENTION_USER);
            LOG.debug("Backups successfully removed");
        }
        LOG.debug("Finished");
    }

    private boolean isEmpty(RetentionEntry retentionEntry) {
        return retentionEntry.getCount() < 1 && retentionEntry.getDays() < 1 && retentionEntry.getSize() < 1;
    }

    private void applySizeRetention(Set<BackupEntry> backupsToRemove, BackupEntry[] backups, RetentionEntry retention) {
        if (retention.getSize() > 0) {
            int size = 0;
            int i;
            for (i = 0; i < backups.length && size < retention.getSize(); i++) {
                size += parseInt(backups[i].getSize());
            }
            for (; i < backups.length; i++) {
                backupsToRemove.add(backups[i]);
            }
        }
    }

    private void applyCountRetention(Set<BackupEntry> backupsToRemove, BackupEntry[] backups, RetentionEntry retention) {
        if (retention.getCount() > 0) {
            for (int i = retention.getCount(); i < backups.length; i++) {
                backupsToRemove.add(backups[i]);
            }
        }
    }

    private void applyDayRetention(Set<BackupEntry> backupsToRemove, BackupEntry[] backups, RetentionEntry retention) {
        if (retention.getDays() > 0) {
            DateTime currentDateTime = new DateTime();
            for (int i = 0; i < backups.length; i++) {
                DateTime creationDate = new DateTime(Long.parseLong(backups[i].getTimeCreated()));
                DateTime expireTime = creationDate.plusDays(retention.getDays());
                if (currentDateTime.isAfter(expireTime)) {
                    backupsToRemove.add(backups[i]);
                }
            }
        }
    }

    private Map<String, Set<BackupEntry>> getBackups() {
        Map<String, Set<BackupEntry>> backupsSortedByCreationTime = new HashMap<>();

        for (BackupEntry backupEntry : backupRepository.findAll()) {
            Set<BackupEntry> backups = backupsSortedByCreationTime.get(backupEntry.getVolumeId());
            if (backups == null) {
                backups = new TreeSet<>(backupComparatorByCreationTime);
                backupsSortedByCreationTime.put(backupEntry.getVolumeId(), backups);
            }
            backups.add(backupEntry);
        }

        return backupsSortedByCreationTime;
    }

    private Map<String, RetentionEntry> getRetentions() {
        Map<String, RetentionEntry> retentionEntryMap = new HashMap<>();

        for (RetentionEntry entry : retentionRepository.findAll()) {
            retentionEntryMap.put(entry.getVolumeId(), entry);
        }

        return retentionEntryMap;
    }

    private int parseInt(String value) {
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            LOG.error(e);
            return 0;
        }
    }

    private static final Comparator<BackupEntry> backupComparatorByCreationTime = new Comparator<BackupEntry>() {
        @Override
        public int compare(BackupEntry o1, BackupEntry o2) {
            return o2.getTimeCreated().compareTo(o1.getTimeCreated());
        }
    };

    public Task getJob(final RetentionService retentionService) {
        return new Task() {
            @Override
            public void run() {
                retentionService.apply();
            }

            @Override
            public String getId() {
                return retentionService.getClass().toString();
            }
        };
    }
}

package com.sungardas.snapdirector.rest;

import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sungardas.snapdirector.aws.PropertyBasedCredentialsProvider;
import com.sungardas.snapdirector.aws.dynamodb.DynamoUtils;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.rest.utils.Constants;
import com.sungardas.snapdirector.service.BackupService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/backup")
public class BackupController {

    private static final Logger LOG = LogManager.getLogger(BackupController.class);

    @Autowired
    private ServletContext context;
    @Autowired
    private HttpServletRequest servletRequest;

    @Autowired
    private BackupService backupService;


    @RequestMapping(value = "/{volumeId}", method = RequestMethod.GET)
    public ResponseEntity<String> get(@PathVariable(value = "volumeId") String volumeId) {
        List<BackupEntry> items = backupService.getBackupList(volumeId);
        LOG.debug("Available backups for volume {}: [{}] .", volumeId, jsonArrayRepresentation(items).toString());
        return new ResponseEntity<>(jsonArrayRepresentation(items).toString(), HttpStatus.OK);
    }

    private JSONArray jsonArrayRepresentation(List<BackupEntry> backupEntries) {
        JSONArray backupsJSONArray = new JSONArray();
        for (BackupEntry entry : backupEntries) {
            JSONObject backupItem = new JSONObject();
            backupItem.put("fileName", entry.getFileName());
            backupItem.put("volumeId", entry.getVolumeId());
            backupItem.put("timeCreated", entry.getTimeCreated());
            backupItem.put("size", entry.getSize());
            backupsJSONArray.put(backupItem);
        }
        return backupsJSONArray;
    }


    @RequestMapping(value = "/{backupName}", method = RequestMethod.DELETE)
    public ResponseEntity<String> deleteBackup(@PathVariable String backupName) {
        LOG.debug("Removing backup [{}]", backupName);
        try{
            backupService.deleteBackup(backupName, getCurrentUserEmail());
            return new ResponseEntity<>(HttpStatus.NO_CONTENT);
        } catch (DataAccessException e){
            return new ResponseEntity<>("Failed to remove backup.", HttpStatus.NOT_ACCEPTABLE);
        }
    }

    private String getCurrentUserEmail() {
        String session = servletRequest.getSession().getId();
        return ((Map<String, String>) context.getAttribute(Constants.CONTEXT_ALLOWED_SESSIONS_ATR_NAME)).get(session);
    }

}

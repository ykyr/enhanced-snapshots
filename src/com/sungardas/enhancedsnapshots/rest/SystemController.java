package com.sungardas.enhancedsnapshots.rest;

import com.sungardas.enhancedsnapshots.components.WorkersDispatcher;
import com.sungardas.enhancedsnapshots.dto.SystemConfiguration;
import com.sungardas.enhancedsnapshots.rest.filters.FilterProxy;
import com.sungardas.enhancedsnapshots.rest.utils.Constants;
import com.sungardas.enhancedsnapshots.service.AWSCommunicationService;
import com.sungardas.enhancedsnapshots.service.ConfigurationService;
import com.sungardas.enhancedsnapshots.service.SDFSStateService;
import com.sungardas.enhancedsnapshots.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.support.XmlWebApplicationContext;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import java.util.Map;


@RestController
@RequestMapping("/system")
public class SystemController {
    @Autowired
    private FilterProxy filterProxy;

    @Autowired
    private HttpServletRequest servletRequest;

    @Autowired
    private ServletContext context;

    @Autowired
    private SDFSStateService sdfsStateService;

    @Autowired
    private ConfigurationService configurationService;

    @Autowired
    private UserService userService;

    @Autowired
    private AWSCommunicationService awsCommunicationService;

    @Autowired
    private XmlWebApplicationContext applicationContext;

    @Autowired
    private WorkersDispatcher workersDispatcher;

    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ResponseEntity<String> deleteService(@RequestBody InstanceId instanceId) {
        String session = servletRequest.getSession().getId();
        String currentUser = ((Map<String, String>) context.getAttribute(Constants.CONTEXT_ALLOWED_SESSIONS_ATR_NAME)).get(session);
        if (!userService.isAdmin(currentUser)) {
            return new ResponseEntity<>("{\"msg\":\"Only admin can delete service\"}", HttpStatus.FORBIDDEN);
        }
        if (!configurationService.getConfigurationId().equals(instanceId.getInstanceId())) {
            return new ResponseEntity<>("{\"msg\":\"Provided instance ID is incorrect\"}", HttpStatus.FORBIDDEN);
        }
        refreshContext();
        return new ResponseEntity<>("", HttpStatus.OK);
    }

    @RequestMapping(method = RequestMethod.GET)
    public ResponseEntity<SystemConfiguration> getSystem() {
        return new ResponseEntity<>(configurationService.getSystemConfiguration(), HttpStatus.OK);
    }

    @RequestMapping(method = RequestMethod.POST)
    public ResponseEntity<String> updateSystemProperties(@RequestBody SystemConfiguration newConfiguration) {
        SystemConfiguration currentConfiguration = configurationService.getSystemConfiguration();
        if (!checkIopsAreValid(newConfiguration.getSystemProperties())) {
            return new ResponseEntity<>("iops per GB can not be less than 1 and more than 30", HttpStatus.BAD_REQUEST);
        }
        if (newConfiguration.getSdfs().getVolumeSize() > currentConfiguration.getSdfs().getMaxVolumeSize()) {
            return new ResponseEntity<>("Volume size can not be more than " + currentConfiguration.getSdfs().getMaxVolumeSize(), HttpStatus.BAD_REQUEST);
        }
        if (newConfiguration.getSdfs().getSdfsLocalCacheSize() > currentConfiguration.getSdfs().getMaxSdfsLocalCacheSize()) {
            return new ResponseEntity<>("Local cache size can not be more than " + currentConfiguration.getSdfs().getMaxSdfsLocalCacheSize(), HttpStatus.BAD_REQUEST);
        }
        boolean needToReconfigureSdfs = false;
        //TODO: move this logic to future SystemService
        if (!configurationService.getS3Bucket().equals(newConfiguration.getS3().getBucketName())) {
            awsCommunicationService.moveDataToNewBucket(configurationService.getS3Bucket(), newConfiguration.getS3().getBucketName());
            awsCommunicationService.dropS3Bucket(configurationService.getS3Bucket());
            needToReconfigureSdfs = true;
        }
        if (configurationService.getSdfsVolumeSizeWithoutMeasureUnit() != newConfiguration.getSdfs().getVolumeSize()) {
            needToReconfigureSdfs = true;
        }
        if (configurationService.getSdfsLocalCacheSizeWithoutMeasureUnit() != newConfiguration.getSdfs().getSdfsLocalCacheSize()) {
            needToReconfigureSdfs = true;
        }
        configurationService.setSystemConfiguration(newConfiguration);
        if (needToReconfigureSdfs) {
            sdfsStateService.reconfigureAndRestartSDFS();
        }
        return new ResponseEntity<>("", HttpStatus.OK);
    }

    @RequestMapping(value = "/backup", method = RequestMethod.GET)
    public ResponseEntity<SystemBackupDto> getConfiguration() {
        return new ResponseEntity<>(new SystemBackupDto(sdfsStateService.getBackupTime()), HttpStatus.OK);
    }

    private void refreshContext() {
        filterProxy.setFilter(null);
        applicationContext.setConfigLocation("/WEB-INF/destroy-spring-web-config.xml");
        applicationContext.getAutowireCapableBeanFactory().destroyBean(workersDispatcher);
        new Thread() {
            @Override
            public void run() {
                applicationContext.refresh();
            }
        }.start();
    }

    private static class SystemBackupDto {
        private Long lastBackup;

        public SystemBackupDto(Long lastBackup) {
            this.lastBackup = lastBackup;
        }

        public Long getLastBackup() {
            return lastBackup;
        }

        public void setLastBackup(Long lastBackup) {
            this.lastBackup = lastBackup;
        }
    }

    private static class InstanceId {

        private String instanceId;

        public String getInstanceId() {
            return instanceId;
        }

        public void setInstanceId(String instanceId) {
            this.instanceId = instanceId;
        }
    }

    // iops per GB can not be less than 1 and more than 30
    private boolean checkIopsAreValid(SystemConfiguration.SystemProperties systemProperties) {
        boolean result = true;
        if (systemProperties.getRestoreVolumeIopsPerGb() > 30 || systemProperties.getTempVolumeIopsPerGb() > 30) {
            result = false;
        }
        if (systemProperties.getRestoreVolumeIopsPerGb() < 1 || systemProperties.getTempVolumeIopsPerGb() < 1) {
            result = false;
        }
        return result;
    }
}

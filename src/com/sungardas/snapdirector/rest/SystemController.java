package com.sungardas.snapdirector.rest;

import com.sungardas.snapdirector.dto.SystemConfiguration;
import com.sungardas.snapdirector.rest.filters.FilterProxy;
import com.sungardas.snapdirector.rest.utils.Constants;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.UserService;
import com.sungardas.snapdirector.service.SDFSStateService;
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
    private XmlWebApplicationContext applicationContext;


    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ResponseEntity<String> deleteService(@RequestBody InstanceId instanceId) {
        String session = servletRequest.getSession().getId();
        String currentUser = ((Map<String, String>) context.getAttribute(Constants.CONTEXT_ALLOWED_SESSIONS_ATR_NAME)).get(session);
        if (!userService.isAdmin(currentUser)) {
            return new ResponseEntity<>("{\"msg\":\"Only admin can delete service\"}", HttpStatus.FORBIDDEN);
        }
        if (!configurationService.getWorkerConfiguration().getConfigurationId().equals(instanceId.getInstanceId())) {
            return new ResponseEntity<>("{\"msg\":\"Provided instance ID is incorrect\"}", HttpStatus.FORBIDDEN);
        }
        refreshContext();
        return new ResponseEntity<>("", HttpStatus.OK);
    }

    @RequestMapping(method = RequestMethod.GET)
    public ResponseEntity<SystemConfiguration> getSystem() {
        return new ResponseEntity<>(configurationService.getSystemConfiguration(), HttpStatus.OK);
    }


    @RequestMapping(value = "/backup", method = RequestMethod.GET)
    public ResponseEntity<SystemBackupDto> getConfiguration() {
        return new ResponseEntity<>(new SystemBackupDto(sdfsStateService.getBackupTime()), HttpStatus.OK);
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

    private void refreshContext() {
        filterProxy.setFilter(null);
        applicationContext.setConfigLocation("/WEB-INF/destroy-spring-web-config.xml");
        new Thread() {
            @Override
            public void run() {
                applicationContext.refresh();
            }
        }.start();
    }
}

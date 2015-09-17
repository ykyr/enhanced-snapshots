package com.sungardas.snapdirector.rest;

import com.amazonaws.util.EC2MetadataUtils;
import com.sungardas.snapdirector.dto.SystemConfiguration;
import com.sungardas.snapdirector.exception.OperationNotAllowedException;
import com.sungardas.snapdirector.rest.utils.Constants;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.RemoveAppConfiguration;
import com.sungardas.snapdirector.service.UserService;
import com.sungardas.snapdirector.service.impl.SDFSStateServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import java.util.Map;


@RestController
@RequestMapping("/system")
public class SystemController {

    @Autowired
    private RemoveAppConfiguration removeAppConfiguration;
    @Autowired
    private HttpServletRequest servletRequest;
    @Autowired
    private ServletContext context;

    @Autowired
    private SDFSStateServiceImpl sdfsStateService;

    @Autowired
    ConfigurationService configurationService;

    @Autowired
    private UserService userService;


    @RequestMapping(method = RequestMethod.DELETE)
    public ResponseEntity<String> deleteService(@RequestBody InstanceID instanceID) {
        String session = servletRequest.getSession().getId();
        String currentUser = ((Map<String, String>) context.getAttribute(Constants.CONTEXT_ALLOWED_SESSIONS_ATR_NAME)).get(session);
        if (!userService.isAdmin(currentUser)) {
            return new ResponseEntity<>("Only admin can delete service", HttpStatus.FORBIDDEN);
        }
        if (!instanceID.instanceID.equals(EC2MetadataUtils.getInstanceId())) {
            return new ResponseEntity<>("Provided instance ID is incorrect", HttpStatus.FORBIDDEN);
        }
        removeAppConfiguration.dropConfiguration(currentUser, instanceID.instanceID);
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

    private static class InstanceID {

        private String instanceID;

        public String getInstanceID() {
            return instanceID;
        }

        public void setInstanceID(String instanceID) {
            this.instanceID = instanceID;
        }
    }
}

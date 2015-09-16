package com.sungardas.snapdirector.rest;

import com.sungardas.snapdirector.dto.SystemConfiguration;
import com.sungardas.snapdirector.exception.OperationNotAllowedException;
import com.sungardas.snapdirector.rest.utils.Constants;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.RemoveAppConfiguration;
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


    @RequestMapping(method = RequestMethod.DELETE)
    public ResponseEntity<String> deleteService(@RequestBody String instanceID) {
        String session = servletRequest.getSession().getId();
        String currentUser = ((Map<String, String>) context.getAttribute(Constants.CONTEXT_ALLOWED_SESSIONS_ATR_NAME)).get(session);
        try {
            removeAppConfiguration.dropConfiguration(currentUser, instanceID);
            return new ResponseEntity<>("", HttpStatus.OK);
        } catch (OperationNotAllowedException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.FORBIDDEN);
        }
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
}

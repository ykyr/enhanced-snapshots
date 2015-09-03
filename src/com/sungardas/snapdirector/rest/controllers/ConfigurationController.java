package com.sungardas.snapdirector.rest.controllers;

import com.sungardas.snapdirector.dto.CredentialsDto;
import com.sungardas.snapdirector.dto.WorkerConfigurationDto;
import com.sungardas.snapdirector.exception.ConfigurationException;
import com.sungardas.snapdirector.service.CredentialsService;
import com.sungardas.snapdirector.service.InitializationService;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import static org.springframework.http.HttpStatus.*;

//@RestController
//@RequestMapping("/configuration")
public class ConfigurationController {
    private static final Log LOG = LogFactory.getLog(ConfigurationController.class);

    @ExceptionHandler(ConfigurationException.class)
    @ResponseBody
    @ResponseStatus(INTERNAL_SERVER_ERROR)
    private Exception internalServerError(ConfigurationException exception){
        LOG.error(exception);
        return exception;
    }

    @Autowired
    CredentialsService credentialsService;

    @Autowired
    InitializationService initializationService;



    @RequestMapping(value="/awscreds", method = RequestMethod.GET)
    public ResponseEntity<String> checkAwsCredentialAreProvided() {
        ResponseEntity<String> responseEntity;
        if (credentialsService.isAwsPropertyFileExists() && credentialsService.areCredentialsValid()) {
            responseEntity = new ResponseEntity<>(OK);
        }else {
            responseEntity = new ResponseEntity<>(NO_CONTENT);
        }
        return responseEntity;
    }

    @RequestMapping(value="/awscreds", method = RequestMethod.POST)
    public ResponseEntity<String> setAwsCredential(@RequestBody CredentialsDto credentials) {
        credentialsService.setCredentials(credentials.getAccessKey(), credentials.getSecretKey());

        if(credentialsService.areCredentialsValid()) {
            return new ResponseEntity<>(OK);
        }
        else {
            throw new ConfigurationException("Provided credentials aren't valid");
        }
    }



    @RequestMapping(value= "/{configurationId}", method = RequestMethod.GET)
    public ResponseEntity<WorkerConfigurationDto> getConfiguration(@PathVariable String configurationId) {
        ResponseEntity<WorkerConfigurationDto> responseEntity=null;
        if(configurationId.equals("predefined")) {
            responseEntity = new ResponseEntity<WorkerConfigurationDto>(getPredefinedConfiguration(), OK);
        }
        return responseEntity;
    }

    private WorkerConfigurationDto getPredefinedConfiguration() {
        initializationService.isDbStructureValid();

        return new WorkerConfigurationDto();
    }

}

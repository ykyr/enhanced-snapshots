package com.sungardas.snapdirector.rest;

import org.springframework.context.annotation.Profile;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@Profile(value="prod")
@RequestMapping("/configuration")
public class ConfigurationController {

    @RequestMapping(value="/queues",method = RequestMethod.GET)
    public ResponseEntity<String> getQueuesList() {
        return null;
    }

    @RequestMapping(value="/mountpoints",method = RequestMethod.GET)
    public ResponseEntity<String> getMountpointsList() {
        return null;
    }

    @RequestMapping(value="/sdfsvolumes",method = RequestMethod.GET)
    public ResponseEntity<String> getSdfsVolumesList() {
        return null;
    }

    @RequestMapping(method = RequestMethod.GET)
    public ResponseEntity<String> getCurrentConfiguration() {
        return null;
    }

    @RequestMapping(value= "/{configurationId}", method = RequestMethod.GET)
    public ResponseEntity<String> getConfiguration(@PathVariable String configurationId) {
        return null;
    }

}

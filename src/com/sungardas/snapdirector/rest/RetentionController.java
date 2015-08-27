package com.sungardas.snapdirector.rest;

import com.sungardas.snapdirector.dto.RetentionDto;
import com.sungardas.snapdirector.exception.SnapdirectorException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import static org.springframework.web.bind.annotation.RequestMethod.GET;
import static org.springframework.web.bind.annotation.RequestMethod.POST;
import static org.springframework.web.bind.annotation.RequestMethod.PUT;

@RestController
@RequestMapping("/retention")
public class RetentionController {

    private static final Logger LOG = LogManager.getLogger(RetentionController.class);

    @ResponseStatus(value = HttpStatus.BAD_REQUEST, reason = "Internal snapdirector error, see logs")
    @ExceptionHandler(SnapdirectorException.class)
    public void exceptionHandler(SnapdirectorException e){
        LOG.error(e);
    }

    @RequestMapping(method = POST)
    public ResponseEntity<Void> modifyRetentionPolicy(@RequestBody RetentionDto retentionDto){
        //TODO call retention service
        return new ResponseEntity(HttpStatus.OK);
    }


    @RequestMapping(value = "/{volumeId}", method = GET)
    public ResponseEntity<RetentionDto> getRetention(@PathVariable String volumeId){
        //TODO call retention service
        return new ResponseEntity<>(new RetentionDto(), HttpStatus.OK);
    }

}

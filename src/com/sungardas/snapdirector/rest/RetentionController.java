package com.sungardas.snapdirector.rest;

import com.sungardas.snapdirector.dto.RetentionDto;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.exception.SnapdirectorException;
import com.sungardas.snapdirector.service.RetentionService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import static org.springframework.web.bind.annotation.RequestMethod.GET;
import static org.springframework.web.bind.annotation.RequestMethod.POST;

@RestController
@RequestMapping("/retention")
public class RetentionController {
    private static final Logger LOG = LogManager.getLogger(RetentionController.class);

    @Autowired
    private RetentionService retentionService;

    @ResponseStatus(value = HttpStatus.BAD_REQUEST, reason = "Internal snapdirector error, see logs")
    @ExceptionHandler(SnapdirectorException.class)
    public void exceptionHandler(SnapdirectorException e){
        LOG.error(e);
    }

    @ResponseStatus(value = HttpStatus.NOT_FOUND, reason = "Volume not found")
    @ExceptionHandler(DataAccessException.class)
    public void dataAccessExceptionHandler(DataAccessException e){
        LOG.error(e);
    }

    @RequestMapping(method = POST)
    public ResponseEntity<Void> modifyRetentionPolicy(@RequestBody RetentionDto retentionDto){
        retentionService.putRetention(retentionDto);
        return new ResponseEntity(HttpStatus.OK);
    }


    @RequestMapping(value = "/{volumeId}", method = GET)
    public ResponseEntity<RetentionDto> getRetention(@PathVariable String volumeId){
        return new ResponseEntity<>(retentionService.getRetentionDto(volumeId), HttpStatus.OK);
    }

}

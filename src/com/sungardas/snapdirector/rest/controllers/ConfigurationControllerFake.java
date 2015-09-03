package com.sungardas.snapdirector.rest.controllers;

import com.amazonaws.util.json.JSONException;
import com.amazonaws.util.json.JSONObject;
import com.sun.jersey.client.urlconnection.HTTPSProperties;
import com.sungardas.snapdirector.dto.CredentialsDto;
import com.sungardas.snapdirector.dto.WorkerConfigurationDto;
import com.sungardas.snapdirector.exception.ConfigurationException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import static org.springframework.http.HttpStatus.NO_CONTENT;
import static org.springframework.http.HttpStatus.OK;

@RequestMapping("/configuration")
public class ConfigurationControllerFake {
    private static final Log LOG = LogFactory.getLog(ConfigurationControllerFake.class);
    private static boolean isCredsProvided = false;

    @RequestMapping(value="/awscreds", method = RequestMethod.GET)
    public ResponseEntity<String> checkAwsCredentialAreProvided() {
        LOG.info("isCredsProvided " + isCredsProvided);
        if(isCredsProvided)
            return new ResponseEntity<>(OK);

        else
            return new ResponseEntity<>(NO_CONTENT);
    }

    @RequestMapping(value="/awscreds", method = RequestMethod.POST)
    public ResponseEntity<String> setAwsCredential(@RequestBody CredentialsDto credentials) {
        if(credentials.getAccessKey().length()==0 ||credentials.getAccessKey().length()==0) {
            throw new ConfigurationException("Provided credentials aren't valid");
        }
        else {
            LOG.info("provided avs keys");
            isCredsProvided = true;
            return new ResponseEntity<>(OK);

        }
    }

    private static boolean isCreated = false;


    @RequestMapping(value= "/current", method = RequestMethod.GET)
    public ResponseEntity<String> getConfiguration() {
        if(!isCredsProvided) {
            new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
        }

        JSONObject conf = new JSONObject();

        try {
            JSONObject s3 = new JSONObject();
            s3.put("isCreated", isCreated);
            s3.put("bucketName", "com.sungardas.snapdirector_" + "i-12f5a345");
            JSONObject queue = new JSONObject();
            s3.put("isCreated", isCreated);
            s3.put("queueName", "snapdirector_" + "i-12f5a345");
            JSONObject sdfs = new JSONObject();
            s3.put("isCreated", isCreated);
            s3.put("volumeName", "awspool");
            s3.put("mountpoint", "/mnt/awspool");
            s3.put("volumesize", "40");
            conf.put("s3",s3);
            conf.put("queue",s3);
            conf.put("sdfs",s3);

        } catch (JSONException e) {
            e.printStackTrace();
        }
       return new ResponseEntity<String>(conf.toString(), HttpStatus.OK);
    }


    @RequestMapping(value= "/current", method = RequestMethod.POST)
    public ResponseEntity<String> setConfiguration(@RequestBody String configuration) {

        isCreated = true;
        JSONObject conf = new JSONObject();

        try {
            JSONObject s3 = new JSONObject();
            s3.put("isCreated", isCreated);
            s3.put("bucketName", "com.sungardas.snapdirector_" + "i-12f5a345");
            JSONObject queue = new JSONObject();
            s3.put("isCreated", isCreated);
            s3.put("queueName", "snapdirector_" + "i-12f5a345");
            JSONObject sdfs = new JSONObject();
            s3.put("isCreated", isCreated);
            s3.put("volumeName", "awspool");
            s3.put("mountpoint", "/mnt/awspool");
            s3.put("volumesize", "40");
            conf.put("s3",s3);
            conf.put("queue",s3);
            conf.put("sdfs",s3);

        } catch (JSONException e) {
            e.printStackTrace();
        }
        return new ResponseEntity<> (conf.toString(),HttpStatus.OK);
    }

}

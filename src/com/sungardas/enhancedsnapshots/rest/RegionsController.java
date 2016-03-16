package com.sungardas.enhancedsnapshots.rest;

import com.amazonaws.regions.Regions;
import com.amazonaws.services.ec2.model.AvailabilityZone;
import com.sungardas.enhancedsnapshots.service.AWSCommunicationService;
import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import java.util.HashSet;
import java.util.Set;


@RestController
public class RegionsController {

    @Autowired
    AWSCommunicationService communicationService;

    @Value("${sungardas.worker.configuration}")
    private String configurationId;

    @RequestMapping(value = "/regions", method = RequestMethod.GET)
    public String getRegions() {
        JSONObject record;
        JSONArray regionsJson = new JSONArray();
        System.out.println("Regions load");
    //    for (Regions nextRegion : Regions.values()) {
    //        Region nextRegion = Region.getRegion(Regions.valueOf("US_EAST_1"));    
    //        System.out.println(nextRegion.getName());
    //      if (currentRegion.equals("ap-northeast-2")) continue;
            record = new JSONObject();
            String region1="us-east-1";
            record.put("id", region1);
            record.put("name", region1);
            regionsJson.put(record);
            record = new JSONObject();
            String region2="us-west-1";
            record.put("id", region2);
            record.put("name", region2);
            regionsJson.put(record);
    //    }
        return regionsJson.toString();
    }

    @RequestMapping( value = "/zones", method = RequestMethod.GET)
    public ResponseEntity getAvailabilityZones() {
        try {
            HashSet<String> availabilityZones = new HashSet<>();
            for (AvailabilityZone zone : communicationService.describeAvailabilityZonesForCurrentRegion()) {
                availabilityZones.add(zone.getZoneName());
            }
            return new ResponseEntity<>(availabilityZones, HttpStatus.OK);
        } catch (Exception e) {
            return new ResponseEntity<>("Failed to get list of availability zones.", HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    @RequestMapping( value = "/zones/current", method = RequestMethod.GET)
    public ResponseEntity getCurrentAvailabilityZone() {
        try {
            JSONObject record = new JSONObject();;
            record.put("zone-name", communicationService.getCurrentAvailabilityZone());

            return new ResponseEntity<String>(record.toString(), HttpStatus.OK);
        } catch (Exception e) {
            return new ResponseEntity<>("Failed to get current availability zone.", HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

}

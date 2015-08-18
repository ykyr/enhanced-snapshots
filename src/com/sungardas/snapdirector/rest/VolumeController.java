package com.sungardas.snapdirector.rest;

import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.sungardas.snapdirector.dto.VolumeDto;
import com.sungardas.snapdirector.service.VolumeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;


@RestController
@RequestMapping("/volume")
public class VolumeController {

    @Autowired
    private VolumeService volumeService;

    @RequestMapping(method = RequestMethod.GET)
    public ResponseEntity getAllVolumes() {
        try {
            List<VolumeDto> volumes = volumeService.getVolumes();
            return new ResponseEntity(volumes, HttpStatus.OK);
        } catch (Exception e) {
            return new ResponseEntity("Failed to get volumes.", HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    @RequestMapping(value = "/{regionId}", method = RequestMethod.GET)
    public ResponseEntity getVolumesByRegion(@PathVariable("regionId") String region) {
        try {
            List<VolumeDto> volumes = volumeService.getVolumesByRegion(Region.getRegion(Regions.fromName(region)));
            return new ResponseEntity(volumes, HttpStatus.OK);
        } catch (Exception e) {
            return new ResponseEntity("Failed to get volumes for region: " + region, HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }
}

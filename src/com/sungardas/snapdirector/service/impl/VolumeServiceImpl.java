package com.sungardas.snapdirector.service.impl;

import com.amazonaws.regions.Region;
import com.amazonaws.services.ec2.AmazonEC2;
import com.sungardas.snapdirector.dto.VolumeDto;
import com.sungardas.snapdirector.dto.converter.VolumeDtoConverter;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.service.VolumeService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;


@Service
public class VolumeServiceImpl implements VolumeService {

    private static final Logger LOG = LogManager.getLogger(VolumeServiceImpl.class);

    @Autowired
    private AmazonEC2 amazonEC2;

    @Override
    public Set<VolumeDto> getVolumes() {
        try {
            LOG.debug("Getting volume list ...");
            Set<VolumeDto> result = VolumeDtoConverter.convert(amazonEC2.describeVolumes().getVolumes());
            LOG.debug("Volume list: [{}]", result);
            return result;
        } catch (RuntimeException e) {
            LOG.error("Failed to get volume list.", e);
            throw new DataAccessException("Failed to get volume list.", e);
        }
    }

    @Override
    public Set<VolumeDto> getVolumesByRegion(Region region) {
        try {
            LOG.debug("Getting volume list for region [{}]", region);
            amazonEC2.setRegion(region);
            Set<VolumeDto> result = VolumeDtoConverter.convert(amazonEC2.describeVolumes().getVolumes());
            LOG.debug("Volume list for region [{}]: [{}]", region, result);
            return result;
        } catch (RuntimeException e) {
            LOG.error("Failed to get volume list by region [{}].", region, e);
            throw new DataAccessException("Failed to get volume list.", e);
        }

    }

    @Override
    public boolean isExists(String volumeId) {
        for(VolumeDto dto: getVolumes()){
            if(dto.getVolumeId().equals(volumeId)){
                return true;
            }
        }
        return false;
    }


}

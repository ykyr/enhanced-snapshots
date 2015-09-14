package com.sungardas.snapdirector.service;

import com.amazonaws.regions.Region;
import com.sungardas.snapdirector.dto.VolumeDto;

import java.util.Set;


public interface VolumeService {
    Set<VolumeDto> getVolumes();

    Set<VolumeDto> getVolumesByRegion(Region region);

    boolean volumeExists(String volumeId);

}

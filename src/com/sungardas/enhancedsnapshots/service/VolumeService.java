package com.sungardas.enhancedsnapshots.service;

import com.amazonaws.regions.Region;
import com.sungardas.enhancedsnapshots.dto.VolumeDto;

import java.util.Set;


public interface VolumeService extends Cached {
    Set<VolumeDto> getVolumes();

    Set<VolumeDto> getVolumesByRegion(Region region);

    boolean volumeExists(String volumeId);

}

package com.sungardas.snapdirector.service;

import com.amazonaws.regions.Region;
import com.sungardas.snapdirector.dto.VolumeDto;

import java.util.List;


public interface VolumeService {
    List<VolumeDto> getVolumes();

    List<VolumeDto> getVolumesByRegion(Region region);

}

package com.sungardas.enhancedsnapshots.dto.converter;

import com.amazonaws.services.ec2.model.Tag;
import com.amazonaws.services.ec2.model.Volume;
import com.sungardas.enhancedsnapshots.dto.VolumeDto;
import org.springframework.beans.BeanUtils;

import java.util.LinkedHashSet;
import java.util.Set;

public class VolumeDtoConverter {

    public static VolumeDto convert(Volume volume) {
        VolumeDto volumeDto = new VolumeDto();
        BeanUtils.copyProperties(volume, volumeDto);
        for (Tag tag : volume.getTags()) {
            // check whether volume has Name tag
            if (tag.getKey().equals("Name")) {
                volumeDto.setVolumeName(tag.getValue());
            }
        }
        // check whether volume is attached to instance
        if (volume.getAttachments().size() > 0) {
            volumeDto.setInstanceID(volume.getAttachments().get(0).getInstanceId());
        }
        return volumeDto;
    }

    public static Set<VolumeDto> convert(Iterable<Volume> volumes) {
        Set<VolumeDto> volumeDtoList = new LinkedHashSet<>();
        for (Volume volume : volumes) {
            volumeDtoList.add(convert(volume));
        }
        return volumeDtoList;
    }
}

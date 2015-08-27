package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.aws.dynamodb.model.RetentionEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.RetentionRepository;
import com.sungardas.snapdirector.dto.RetentionDto;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.service.RetentionService;
import com.sungardas.snapdirector.service.VolumeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import static com.sungardas.snapdirector.dto.converter.RetentionConverter.toDto;
import static com.sungardas.snapdirector.dto.converter.RetentionConverter.toEntry;

@Service
public class RetentionServiceImpl implements RetentionService {

    @Autowired
    private RetentionRepository retentionRepository;

    @Autowired
    private VolumeService volumeService;

    @Override
    public void putRetention(RetentionDto retentionDto) {
        if(volumeService.isExists(retentionDto.getVolumeId())) {
            RetentionEntry entry = toEntry(retentionDto);

            retentionRepository.save(entry);
        } else {
            throw new DataAccessException("Volume with id:" +retentionDto.getVolumeId()+" not found");
        }
    }

    @Override
    public RetentionDto getRetentionDto(String volumeId) {
        RetentionEntry entry = retentionRepository.findOne(volumeId);

        if (entry != null){
            return toDto(entry);
        } else {
            if(volumeService.isExists(volumeId)){
                return new RetentionDto(volumeId);
            } else {
                throw new DataAccessException("Volume with id:" +volumeId+" not found");
            }
        }
    }
}

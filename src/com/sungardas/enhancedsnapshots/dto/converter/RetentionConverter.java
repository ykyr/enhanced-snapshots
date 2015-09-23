package com.sungardas.enhancedsnapshots.dto.converter;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.RetentionEntry;
import com.sungardas.enhancedsnapshots.dto.RetentionDto;
import org.springframework.beans.BeanUtils;

public final class RetentionConverter {
    private RetentionConverter() {
    }

    public static RetentionDto toDto(RetentionEntry entry){
        RetentionDto dto = new RetentionDto();

        BeanUtils.copyProperties(entry, dto);

        return dto;
    }

    public static RetentionEntry toEntry(RetentionDto dto){
        RetentionEntry retentionEntry = new RetentionEntry();

        BeanUtils.copyProperties(dto, retentionEntry);

        return retentionEntry;
    }
}

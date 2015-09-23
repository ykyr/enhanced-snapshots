package com.sungardas.enhancedsnapshots.service;

import com.sungardas.enhancedsnapshots.dto.RetentionDto;

public interface RetentionService {
    void putRetention(RetentionDto retentionDto);

    RetentionDto getRetentionDto(String volumeId);

    void apply();
}

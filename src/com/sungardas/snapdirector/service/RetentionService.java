package com.sungardas.snapdirector.service;

import com.sungardas.snapdirector.dto.RetentionDto;

public interface RetentionService {
    void putRetention(RetentionDto retentionDto);

    RetentionDto getRetentionDto(String volumeId);

    void apply();

    void deleteAllRetentions();

    boolean isTableEmpty();
}

package com.sungardas.snapdirector.service.impl;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile("dev")
public class InitializationServiceFake extends InitializationServiceImpl {
    @Value("${sungardas.worker.configuration}")
    private String fakeConfigurationId;

    protected String getConfigurationId() {
        return fakeConfigurationId;
    }
}

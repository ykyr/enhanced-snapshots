package com.sungardas.enhancedsnapshots.service.impl;


import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile("dev")
public class ConfigurationServiceDev extends ConfigurationServiceImpl {

    protected String getInstanceId() {
        return "DEV";
    }
}

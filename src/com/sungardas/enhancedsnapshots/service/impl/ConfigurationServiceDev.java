package com.sungardas.enhancedsnapshots.service.impl;


import com.sungardas.enhancedsnapshots.dto.SystemConfiguration;
import com.sungardas.utils.SdfsUtils;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile("dev")
public class ConfigurationServiceDev extends ConfigurationServiceImpl {

    protected String getInstanceId() {
        return "DEV";
    }

    @Override
    public SystemConfiguration getSystemConfiguration() {
        SystemConfiguration configuration = super.getSystemConfiguration();
        configuration.getSdfs().setMaxVolumeSize(1500);
        configuration.getSdfs().setMaxSdfsLocalCacheSize(30);
        return configuration;
    }
}

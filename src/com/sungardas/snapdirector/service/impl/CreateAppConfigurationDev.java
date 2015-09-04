package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.service.CreateAppConfiguration;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Profile("dev")
@Service
public class CreateAppConfigurationDev implements CreateAppConfiguration {
}

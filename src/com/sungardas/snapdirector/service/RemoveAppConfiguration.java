package com.sungardas.snapdirector.service;

public interface RemoveAppConfiguration {
    String getConfigurationId();

    void dropConfiguration(String currentUserEmail, String instanceId);
}

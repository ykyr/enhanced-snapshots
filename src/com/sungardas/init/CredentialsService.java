package com.sungardas.init;

import com.sungardas.snapdirector.dto.InitConfigurationDto;

import javax.validation.constraints.NotNull;

interface CredentialsService {
    void setCredentialsIfValid(@NotNull CredentialsDto credentials);

    void storeCredentials();

    boolean areCredentialsValid();

    InitConfigurationDto getInitConfigurationDto();

    boolean isAwsPropertyFileExists();

    boolean checkDefaultUser(String login, String password);

    String getInstanceId();
}

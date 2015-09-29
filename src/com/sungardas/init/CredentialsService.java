package com.sungardas.init;

import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;

import javax.validation.constraints.NotNull;

interface CredentialsService {
    void setCredentialsIfValid(@NotNull CredentialsDto credentials);

    void storeCredentials();

    void removeCredentials();

    boolean areCredentialsValid();

    boolean credentialsAreProvided();

    InitConfigurationDto getInitConfigurationDto();

    boolean isAwsPropertyFileExists();

    boolean checkDefaultUser(String login, String password);

    String getInstanceId();
}

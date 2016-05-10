package com.sungardas.init;

import javax.validation.constraints.NotNull;

import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;

interface CredentialsService {
    void setCredentialsIfValid(@NotNull CredentialsDto credentials);

    void storeProperties();

    void removeProperties();

    boolean areCredentialsValid();

    boolean credentialsAreProvided();

    InitConfigurationDto getInitConfigurationDto();

    boolean isAwsPropertyFileExists();

    boolean checkDefaultUser(String login, String password);

    String getInstanceId();

    void configureAWSLogAgent();

    void validateVolumeSize(String volumeSize);
}

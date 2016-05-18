package com.sungardas.init;

import javax.validation.constraints.NotNull;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;

interface InitConfigurationService {
    void setCredentialsIfValid(@NotNull CredentialsDto credentials);

    void removeProperties();

    boolean areCredentialsValid();

    boolean credentialsAreProvided();

    InitConfigurationDto getInitConfigurationDto();

    boolean propertyFileExists();

    boolean checkDefaultUser(String login, String password);

    String getInstanceId();

    void configureAWSLogAgent();

    void storePropertiesEditableFromConfigFile();

    void setUser(User user);

    void setInitConfigurationDto(InitConfigurationDto initConfigurationDto);

    void createDBAndStoreSettings(final InitController.ConfigDto config);

    void syncSettingsInDbAndConfigFile();

    void validateVolumeSize(String volumeSize);
}

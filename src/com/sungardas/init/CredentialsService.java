package com.sungardas.init;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;
import com.sungardas.enhancedsnapshots.dto.UserDto;

import javax.validation.constraints.NotNull;

interface CredentialsService {
    void setCredentialsIfValid(@NotNull CredentialsDto credentials);

    void removeCredentials();

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

    void createDBAndStoreSettings();

    void syncSettingsInDbAndConfigFile();
}

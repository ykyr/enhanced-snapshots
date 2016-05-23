package com.sungardas.init;


import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;

interface InitConfigurationService {

    void removeProperties();

    InitConfigurationDto getInitConfigurationDto();

    boolean propertyFileExists();

    boolean checkDefaultUser(String login, String password);

    String getInstanceId();

    void configureAWSLogAgent();

    void storePropertiesEditableFromConfigFile();

    void setUser(User user);

    void createDBAndStoreSettings(final InitController.ConfigDto config);

    void syncSettingsInDbAndConfigFile();

    void validateVolumeSize(int volumeSize);
}

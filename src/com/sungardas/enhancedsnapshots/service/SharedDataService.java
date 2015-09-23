package com.sungardas.enhancedsnapshots.service;


import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;
import com.sungardas.enhancedsnapshots.dto.UserDto;

public interface SharedDataService {
    InitConfigurationDto getInitConfigurationDto();

    UserDto getAdminUser();

    String getAdminPassword();

    void setUser(User user);

    void setInitConfigurationDto(InitConfigurationDto initConfigurationDto);
}

package com.sungardas.snapdirector.service;


import com.sungardas.snapdirector.aws.dynamodb.model.User;
import com.sungardas.snapdirector.dto.InitConfigurationDto;
import com.sungardas.snapdirector.dto.UserDto;

public interface SharedDataService {
    InitConfigurationDto getInitConfigurationDto();

    UserDto getAdminUser();

    String getAdminPassword();

    void setUser(User user);

    void setInitConfigurationDto(InitConfigurationDto initConfigurationDto);
}

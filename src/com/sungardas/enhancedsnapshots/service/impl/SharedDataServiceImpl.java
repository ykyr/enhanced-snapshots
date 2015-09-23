package com.sungardas.enhancedsnapshots.service.impl;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;
import com.sungardas.enhancedsnapshots.dto.UserDto;
import com.sungardas.enhancedsnapshots.dto.converter.UserDtoConverter;
import com.sungardas.enhancedsnapshots.service.SharedDataService;

public class SharedDataServiceImpl implements SharedDataService {

    private InitConfigurationDto initConfigurationDto;

    private UserDto userDto;

    private String password;

    @Override
    public InitConfigurationDto getInitConfigurationDto() {
        return initConfigurationDto;
    }

    @Override
    public UserDto getAdminUser() {
        return userDto;
    }

    @Override
    public String getAdminPassword() {
        return password;
    }

    @Override
    public void setUser(User user) {
        if (user != null) {
            userDto = UserDtoConverter.convert(user);
            userDto.setRole("admin");
            password = user.getPassword();
        }
    }

    @Override
    public void setInitConfigurationDto(InitConfigurationDto initConfigurationDto) {
        this.initConfigurationDto = initConfigurationDto;
    }

}

package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.dto.InitConfigurationDto;
import com.sungardas.snapdirector.dto.UserDto;
import com.sungardas.snapdirector.service.SharedDataService;

public class SharedDataserviceImpl implements SharedDataService {

    @Override
    public InitConfigurationDto getInitConfigurationDto() {
        return null;
    }

    @Override
    public UserDto getAdminUser() { return null;}
    @Override
    public String getAdminPassword() {return null;}

}

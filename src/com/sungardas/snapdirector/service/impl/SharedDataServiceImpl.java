package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.dto.InitConfigurationDto;
import com.sungardas.snapdirector.dto.UserDto;
import com.sungardas.snapdirector.exception.ConfigurationException;
import com.sungardas.snapdirector.service.SharedDataService;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.node.ObjectNode;

import java.io.IOException;

public class SharedDataServiceImpl implements SharedDataService {

    private InitConfigurationDto initConfigurationDto;

    private UserDto userDto;

    private String password;

    @Override
    public InitConfigurationDto getInitConfigurationDto() {
        return initConfigurationDto;
    }

    @Override
    public UserDto getAdminUser() { return userDto;}

    @Override
    public String getAdminPassword() {return password;}

    @Override
    public void setUserInfo(String userInfo) {
        if(userInfo != null) {
            ObjectMapper mapper = new ObjectMapper();
            mapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            try {
                userDto = mapper.readValue(userInfo, UserDto.class);
                password = mapper.readValue(userInfo, ObjectNode.class).get("password").asText();
            } catch (IOException e) {
                throw new ConfigurationException(e);
            }
        }
    }

    @Override
    public void setInitConfigurationDto(InitConfigurationDto initConfigurationDto) {
        this.initConfigurationDto = initConfigurationDto;
    }

}

package com.sungardas.enhancedsnapshots.service;


import com.amazonaws.auth.AWSCredentials;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;
import com.sungardas.enhancedsnapshots.dto.UserDto;

public interface SharedDataService {
    InitConfigurationDto getInitConfigurationDto();

    UserDto getAdminUser();

    String getAdminPassword();

    void setUser(User user);

    void setAWSCredentials(AWSCredentials awsCredentials);

    AWSCredentials getAWSCredentials();

    void setInitConfigurationDto(InitConfigurationDto initConfigurationDto);
}

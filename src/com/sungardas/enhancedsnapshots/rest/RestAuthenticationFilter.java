package com.sungardas.enhancedsnapshots.rest;

import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.UserRepository;

import javax.servlet.Filter;

public interface RestAuthenticationFilter extends Filter {
    void setUserRepository(UserRepository userRepository);

    void setInstanceId(String instanceId);
}

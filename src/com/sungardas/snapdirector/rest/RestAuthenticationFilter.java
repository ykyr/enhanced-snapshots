package com.sungardas.snapdirector.rest;

import com.sungardas.snapdirector.aws.dynamodb.repository.UserRepository;

import javax.servlet.Filter;

public interface RestAuthenticationFilter extends Filter {
    void setUserRepository(UserRepository userRepository);

    void setInstanceId(String instanceId);
}

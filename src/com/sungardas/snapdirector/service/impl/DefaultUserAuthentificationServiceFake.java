package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.service.DefaultUserAuthenticationService;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile(value = "dev")
public class DefaultUserAuthentificationServiceFake implements DefaultUserAuthenticationService {
    private static final Log LOG = LogFactory.getLog(DefaultUserAuthentificationServiceFake.class);
    private final String DEFAULT_LOGIN = "admin@snapdirector";
    private final String FAKE_PWD = "admin@snapdirector";

    @Override
    public boolean checkDefaultUser(String login, String password)
    {
        return login.equals(DEFAULT_LOGIN) && password.equals(FAKE_PWD);
    }
}

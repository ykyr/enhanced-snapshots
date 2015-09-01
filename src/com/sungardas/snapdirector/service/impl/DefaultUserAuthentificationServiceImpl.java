package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.exception.SnapdirectorException;
import com.sungardas.snapdirector.service.DefaultUserAuthenticationService;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Scanner;

@Service
@Profile(value = "prod")
public class DefaultUserAuthentificationServiceImpl implements DefaultUserAuthenticationService {
    private static final Log LOG = LogFactory.getLog(DefaultUserAuthentificationServiceImpl.class);
    private final String DEFAULT_LOGIN = "admin@snapdirector";

    @Override
    public boolean checkDefaultUser(String login, String passwd)
    {   String configId = getConfigurationId();
        return login.equals(DEFAULT_LOGIN) && passwd.equals(configId);
    }

    protected String getConfigurationId() {
        String instanceId = null;
        try {
            URL url = new URL("http://169.254.169.254/latest/meta-data/instance-id");
            URLConnection conn = url.openConnection();
            Scanner s = new Scanner(conn.getInputStream());
            if (s.hasNext()) {
                instanceId = s.next();
                LOG.info("Getting configuration id from metadata: " + instanceId);
            }
            s.close();
        } catch (IOException e) {
            LOG.warn("Failed to determine ec2 instance ID");
            throw new SnapdirectorException("Failed to determine ec2 instance ID", e);
        }
        return instanceId;
    }
}

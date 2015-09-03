package com.sungardas.snapdirector.rest.controllers;

import com.amazonaws.util.json.JSONException;
import com.amazonaws.util.json.JSONObject;
import com.sungardas.snapdirector.aws.dynamodb.model.User;
import com.sungardas.snapdirector.dto.CredentialsDto;
import com.sungardas.snapdirector.dto.UserDto;
import com.sungardas.snapdirector.exception.ConfigurationException;
import com.sungardas.snapdirector.exception.SnapdirectorException;
import com.sungardas.snapdirector.rest.filters.FilterProxy;
import com.sungardas.snapdirector.service.CredentialsService;
import com.sungardas.snapdirector.service.DefaultUserAuthenticationService;
import com.sungardas.snapdirector.service.InitializationService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.ObjectFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.support.XmlWebApplicationContext;

import javax.annotation.PostConstruct;
import javax.servlet.Filter;
import java.net.URI;
import java.net.URISyntaxException;

import static org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR;
import static org.springframework.http.HttpStatus.OK;


@RestController
public class InitControllerDev implements ApplicationContextAware, InitContextListener {

    private static final Logger LOG = LogManager.getLogger(InitControllerDev.class);

    @Autowired
    private FilterProxy filterProxy;

    @Autowired
    private ObjectFactory<InitializationService> initializationServiceObjectFactory;

    @Autowired
    private CredentialsService credentialsService;

    @Autowired
    private DefaultUserAuthenticationService defaultUserAuthenticationService;

    @Autowired
    private XmlWebApplicationContext applicationContext;

    private boolean awsPropertyFileExists = false;

    private boolean isCredsProvided = false;

    private boolean CONTEXT_REFRESH_IN_PROCESS = false;

    @ExceptionHandler(SnapdirectorException.class)
    @ResponseBody
    @ResponseStatus(INTERNAL_SERVER_ERROR)
    private Exception internalServerError(SnapdirectorException exception) {
        LOG.error(exception);
        return exception;
    }

    @RequestMapping(method = RequestMethod.POST, value = "/session")
    public ResponseEntity<String> init(@RequestBody User user) {
        ResponseEntity<String> responseEntity = null;
        if (CONTEXT_REFRESH_IN_PROCESS) {
            responseEntity = new ResponseEntity<>("", HttpStatus.NO_CONTENT);
        }
        // check that aws credentials are provided
        // try to authenticate as real admin user
        else if (awsPropertyFileExists && credentialsService.areCredentialsValid()) {
            LOG.info("Valid aws credentials were provided.");
            refreshContext();
            // update initialization service
            InitializationService initializationService = initializationServiceObjectFactory.getObject();
            if (initializationService.isAdminUserExists()) {
                HttpHeaders headers = new HttpHeaders();

                try {
                    headers.setLocation(new URI("/rest/session"));
                } catch (URISyntaxException e) {
                    e.printStackTrace();
                }
                responseEntity = new ResponseEntity<>(headers, HttpStatus.CREATED);
            }
        }

        // no aws credentials are provided
        // try to authenticate as default user admin@snapdirector:<instance-id>
        else if (defaultUserAuthenticationService.checkDefaultUser(user.getEmail(), user.getPassword())) {
            responseEntity = new ResponseEntity<>("{ \"role\":\"configurator\" }", HttpStatus.OK);
        } else {
            responseEntity = new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
        }
        return responseEntity;
    }

    @RequestMapping(value = "/configuration/awscreds", method = RequestMethod.POST)
    public ResponseEntity<String> setAwsCredential(@RequestBody CredentialsDto credentials) {
        if (credentials.getAccessKey().length() == 0 || credentials.getAccessKey().length() == 0) {
            throw new ConfigurationException("Provided credentials aren't valid");
        } else {
            LOG.info("provided avs keys");
            isCredsProvided = true;
            return new ResponseEntity<>(OK);
        }
    }


    @RequestMapping(value = "/configuration/current", method = RequestMethod.GET)
    public ResponseEntity<InitConfigurationDto> getConfiguration() {
        if (!isCredsProvided) {
            throw new ConfigurationException("Credentials aren't present");
        }
        return new ResponseEntity<>(getConfig(), HttpStatus.OK);
    }


    @RequestMapping(value = "/configuration/current", method = RequestMethod.POST)
    public ResponseEntity<String> setConfiguration(@RequestBody String userInfo) {
        if(userInfo == null && !userInfo.isEmpty() && !getConfig().getDb().isValid()){
            throw new ConfigurationException("Please create default user");
        }
        refreshContext();
        return new ResponseEntity<>("", HttpStatus.OK);
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = (XmlWebApplicationContext) applicationContext;
    }

    private void refreshContext() {
        LOG.info("Context refresh process started.");
        CONTEXT_REFRESH_IN_PROCESS = true;

        applicationContext.setConfigLocation("/WEB-INF/spring-web-config.xml");
        applicationContext.refresh();

        // enabling auth filter
        filterProxy.setFilter((Filter) applicationContext.getBean("restAuthenticationFilter"));

        LOG.info("Context refreshed successfully.");
        CONTEXT_REFRESH_IN_PROCESS = false;
    }

    private InitConfigurationDto getConfig(){
        InitConfigurationDto config = new InitConfigurationDto();

        InitConfigurationDto.S3 s3 = new InitConfigurationDto.S3();
        s3.setBucketName("com.sungardas.snapdirector_i-12f5a345");
        s3.setCreated(true);

        InitConfigurationDto.SDFS sdfs = new InitConfigurationDto.SDFS();
        sdfs.setCreated(true);
        sdfs.setMountPoint("/mnt/awspool");
        sdfs.setVolumeName("awspool");
        sdfs.setVolumeSize("40");

        InitConfigurationDto.Queue queue = new InitConfigurationDto.Queue();
        queue.setQueueName("snapdirector_i-12f5a345");
        queue.setCreated(true);

        InitConfigurationDto.DB db = new InitConfigurationDto.DB();
        db.setValid(false);

        config.setS3(s3);
        config.setSdfs(sdfs);
        config.setQueue(queue);
        config.setDb(db);

        return config;
    }

    @Override
    public boolean isContextRefreshInProcess() {
        return CONTEXT_REFRESH_IN_PROCESS;
    }
}

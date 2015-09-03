package com.sungardas.init.controllers;

import java.net.URI;
import java.net.URISyntaxException;

import javax.annotation.PostConstruct;
import javax.servlet.Filter;

import com.sungardas.snapdirector.aws.dynamodb.Roles;
import com.sungardas.snapdirector.aws.dynamodb.model.User;
import com.sungardas.snapdirector.dto.CredentialsDto;
import com.sungardas.snapdirector.dto.WorkerConfigurationDto;
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
import org.springframework.context.annotation.Profile;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.support.XmlWebApplicationContext;

import static org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR;
import static org.springframework.http.HttpStatus.NO_CONTENT;
import static org.springframework.http.HttpStatus.OK;

@Profile("prod")
@RestController
public class InitController implements ApplicationContextAware {

    private static final Logger LOG = LogManager.getLogger(InitController.class);

    @Autowired
    private FilterProxy filterProxy;

    @Autowired
    private ObjectFactory<InitializationService> initializationServiceObjectFactory;

    @Autowired
    private CredentialsService credentialsService;

    @Autowired
    private DefaultUserAuthenticationService defaultUserAuthenticationService;

    @Autowired
    private ApplicationContext applicationContext;

    @Autowired
    private InitializationService initializationService;

    private boolean awsPropertyFileExists = false;

    private boolean CONTEXT_REFRESH_IN_PROCESS = false;

    @PostConstruct
    public void init() {
        awsPropertyFileExists = credentialsService.isAwsPropertyFileExists();
        if (awsPropertyFileExists && credentialsService.areCredentialsValid()) {
            refreshContext();
        }
    }

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
            User defaultUser = new User();
            defaultUser.setRole(Roles.CONFIGURATOR.getName());
            String result = defaultUser.toString();
            responseEntity = new ResponseEntity<>(result, HttpStatus.OK);
        } else {
            responseEntity = new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
        }
        return responseEntity;
    }

    @RequestMapping(value = "/awscreds", method = RequestMethod.GET)
    public ResponseEntity<String> checkAwsCredentialAreProvided() {
        ResponseEntity<String> responseEntity;
        if (credentialsService.isAwsPropertyFileExists() && credentialsService.areCredentialsValid()) {
            responseEntity = new ResponseEntity<>(OK);
        } else {
            responseEntity = new ResponseEntity<>(NO_CONTENT);
        }
        return responseEntity;
    }

    @RequestMapping(value = "/awscreds", method = RequestMethod.POST)
    public ResponseEntity<String> setAwsCredential(@RequestBody CredentialsDto credentials) {
        credentialsService.setCredentials(credentials.getAwsPublicKey(), credentials.getAwsSecretKey());

        if (credentialsService.areCredentialsValid()) {
            return new ResponseEntity<>(OK);
        } else {
            throw new ConfigurationException("Provided credentials aren't valid");
        }
    }

    @RequestMapping(value = "/{configurationId}", method = RequestMethod.GET)
    public ResponseEntity<WorkerConfigurationDto> getConfiguration(@PathVariable String configurationId) {
        ResponseEntity<WorkerConfigurationDto> responseEntity = null;
        if (configurationId.equals("predefined")) {
            responseEntity = new ResponseEntity<WorkerConfigurationDto>(getPredefinedConfiguration(), OK);
        }
        return responseEntity;
    }

    private WorkerConfigurationDto getPredefinedConfiguration() {
        initializationService.isDbStructureValid();

        return new WorkerConfigurationDto();
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }

    private void refreshContext() {
        LOG.info("Context refresh process started.");
        CONTEXT_REFRESH_IN_PROCESS = true;

        ((XmlWebApplicationContext) applicationContext).setConfigLocation("/WEB-INF/spring-web-config.xml");
        ((XmlWebApplicationContext) applicationContext).refresh();

        // enabling auth filter
        filterProxy.setFilter((Filter) applicationContext.getBean("restAuthenticationFilter"));

        LOG.info("Context refreshed successfully.");
        CONTEXT_REFRESH_IN_PROCESS = false;
    }
}

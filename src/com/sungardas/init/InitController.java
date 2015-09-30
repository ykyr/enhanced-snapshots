package com.sungardas.init;

import com.amazonaws.AmazonClientException;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.UserRepository;
import com.sungardas.enhancedsnapshots.dto.InitConfigurationDto;
import com.sungardas.enhancedsnapshots.exception.ConfigurationException;
import com.sungardas.enhancedsnapshots.exception.EnhancedSnapshotsException;
import com.sungardas.enhancedsnapshots.rest.RestAuthenticationFilter;
import com.sungardas.enhancedsnapshots.rest.filters.FilterProxy;
import com.sungardas.enhancedsnapshots.service.SharedDataService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.support.XmlWebApplicationContext;

import javax.annotation.PostConstruct;
import java.util.Arrays;

import static org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR;
import static org.springframework.http.HttpStatus.OK;


@RestController
class InitController implements ApplicationContextAware {

    private static final Logger LOG = LogManager.getLogger(InitController.class);

    @Autowired
    private FilterProxy filterProxy;

    @Autowired
    private CredentialsService credentialsService;

    @Autowired
    private SharedDataService sharedDataService;

    @Autowired
    private XmlWebApplicationContext applicationContext;

    private boolean CONTEXT_REFRESH_IN_PROCESS = false;

    @PostConstruct
    private void init() {
        // check that aws credentials are provided
        // try to authenticate as real admin user
        if (credentialsService.areCredentialsValid() && credentialsService.isAwsPropertyFileExists()) {
            LOG.info("Valid aws credentials were provided.");
            refreshContext();
        }
    }

    @ExceptionHandler(value = {EnhancedSnapshotsException.class, ConfigurationException.class})
    @ResponseBody
    @ResponseStatus(INTERNAL_SERVER_ERROR)
    private Exception internalServerError(Exception exception) {
        LOG.error(exception);
        return exception;
    }


    @ExceptionHandler(value = {Exception.class, AmazonClientException.class})
    @ResponseBody
    @ResponseStatus(INTERNAL_SERVER_ERROR)
    private Exception amazonException(Exception exception) {
        LOG.error(exception);
        return new EnhancedSnapshotsException("Internal server error", exception);
    }

    @RequestMapping(method = RequestMethod.POST, value = "/session")
    public ResponseEntity<String> init(@RequestBody User user) {
        if (CONTEXT_REFRESH_IN_PROCESS) {
            return new ResponseEntity<>("", HttpStatus.NOT_FOUND);
        }
        // no aws credentials are provided
        // try to authenticate as default user admin@enhancedsnapshots:<instance-id>
        else if (credentialsService.checkDefaultUser(user.getEmail(), user.getPassword())) {
            return new ResponseEntity<>("{ \"role\":\"configurator\" }", HttpStatus.OK);
        } else {
            return new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
        }
    }

    @RequestMapping(value = "/configuration/awscreds", method = RequestMethod.POST)
    public ResponseEntity<String> setAwsCredential(@RequestBody CredentialsDto credentials) {
        credentialsService.setCredentialsIfValid(credentials);
        LOG.info("provided aws keys");
        return new ResponseEntity<>(OK);
    }

    @RequestMapping(value = "/configuration/awscreds", method = RequestMethod.GET)
    public ResponseEntity<String> getAwsCredentialsInfo() {
        if (credentialsService.credentialsAreProvided()) {
            return new ResponseEntity<>("{\"contains\": true}", HttpStatus.OK);
        } else {
            return new ResponseEntity<>("{\"contains\": false}", HttpStatus.OK);
        }
    }


    @RequestMapping(value = "/configuration/current", method = RequestMethod.GET)
    public ResponseEntity<InitConfigurationDto> getConfiguration() {
        return new ResponseEntity<>(credentialsService.getInitConfigurationDto(), HttpStatus.OK);
    }


    @RequestMapping(value = "/configuration/current", method = RequestMethod.POST)
    public ResponseEntity<String> setConfiguration(@RequestBody ConfigDto config) {
        if (credentialsService.areCredentialsValid()) {
            InitConfigurationDto initConfigurationDto = credentialsService.getInitConfigurationDto();
            if (!initConfigurationDto.getDb().isValid()) {
                if (config.getUser() == null) {
                    throw new ConfigurationException("Please create default user");
                }
                sharedDataService.setUser(config.getUser());
            }
            if (config.getUser() != null) {
                sharedDataService.setUser(config.getUser());
            }
            initConfigurationDto.setS3(Arrays.asList(new InitConfigurationDto.S3(config.getBucketName(), false)));
            sharedDataService.setInitConfigurationDto(initConfigurationDto);
            credentialsService.storeCredentials();

            try {
                refreshContext();
            } catch (Exception e) {
                credentialsService.removeCredentials();
                throw e;
            }
            return new ResponseEntity<>("", HttpStatus.OK);
        } else {
            throw new ConfigurationException("AWS credentials invalid");
        }
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
        RestAuthenticationFilter filter = applicationContext.getBean(RestAuthenticationFilter.class);
        filter.setUserRepository(applicationContext.getBean(UserRepository.class));
        filter.setInstanceId(credentialsService.getInstanceId());
        filterProxy.setFilter(filter);

        LOG.info("Context refreshed successfully.");
        CONTEXT_REFRESH_IN_PROCESS = false;
    }

    private static class ConfigDto {
        private User user;
        private String bucketName;

        public User getUser() {
            return user;
        }

        public void setUser(User user) {
            this.user = user;
        }

        public String getBucketName() {
            return bucketName;
        }

        public void setBucketName(String bucketName) {
            this.bucketName = bucketName;
        }
    }
}

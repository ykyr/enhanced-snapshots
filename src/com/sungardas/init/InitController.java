package com.sungardas.init;

import java.util.Arrays;

import javax.annotation.PostConstruct;

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
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.support.XmlWebApplicationContext;

import static org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR;
import static org.springframework.http.HttpStatus.OK;


@RestController
class InitController implements ApplicationContextAware {

    private static final Logger LOG = LogManager.getLogger(InitController.class);
    private static final String GB_UNIT = "GB";

    @Autowired
    private FilterProxy filterProxy;

    @Autowired
    private InitConfigurationService initConfigurationService;

    @Autowired
    private SharedDataService sharedDataService;

    @Autowired
    private XmlWebApplicationContext applicationContext;

    private boolean CONTEXT_REFRESH_IN_PROCESS = false;

    @PostConstruct
    private void init() {
        // check that aws credentials are provided
        // try to authenticate as real admin user
        if (initConfigurationService.isAwsPropertyFileExists()) {
            LOG.info("Valid aws credentials were provided.");
            refreshContext();
        } else {
            initConfigurationService.configureAWSLogAgent();

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
        else if (initConfigurationService.checkDefaultUser(user.getEmail(), user.getPassword())) {
            return new ResponseEntity<>("{ \"role\":\"configurator\" }", HttpStatus.OK);
        } else {
            return new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
        }
    }

    @RequestMapping(value = "/configuration/awscreds", method = RequestMethod.POST)
    public ResponseEntity<String> setAwsCredential(@RequestBody CredentialsDto credentials) {
        initConfigurationService.setCredentialsIfValid(credentials);
        LOG.info("provided aws keys");
        return new ResponseEntity<>(OK);
    }

    @RequestMapping(value = "/configuration/awscreds", method = RequestMethod.GET)
    public ResponseEntity<String> getAwsCredentialsInfo() {
        if (initConfigurationService.credentialsAreProvided()) {
            return new ResponseEntity<>("{\"contains\": true}", HttpStatus.OK);
        } else {
            return new ResponseEntity<>("{\"contains\": false}", HttpStatus.OK);
        }
    }


    @RequestMapping(value = "/configuration/current", method = RequestMethod.GET)
    public ResponseEntity<InitConfigurationDto> getConfiguration() {
        return new ResponseEntity<>(initConfigurationService.getInitConfigurationDto(), HttpStatus.OK);
    }


    @RequestMapping(value = "/configuration/current", method = RequestMethod.POST)
    public ResponseEntity<String> setConfiguration(@RequestBody ConfigDto config) {
        if (initConfigurationService.areCredentialsValid()) {
            InitConfigurationDto initConfigurationDto = initConfigurationService.getInitConfigurationDto();
            if (!initConfigurationDto.getDb().isValid()) {
                if (config.getUser() == null) {
                    throw new ConfigurationException("Please create default user");
                }
                sharedDataService.setUser(config.getUser());
            }
            if (config.getUser() != null) {
                sharedDataService.setUser(config.getUser());
            }
            initConfigurationService.validateVolumeSize(config.getVolumeSize());
            initConfigurationDto.getSdfs().setVolumeSize(config.getVolumeSize() + GB_UNIT);
            initConfigurationDto.setS3(Arrays.asList(new InitConfigurationDto.S3(config.getBucketName(), false)));
            sharedDataService.setInitConfigurationDto(initConfigurationDto);
            initConfigurationService.storeProperties();

            try {
                refreshContext();
            } catch (Exception e) {
                initConfigurationService.removeProperties();
                throw e;
            }
            return new ResponseEntity<>("", HttpStatus.OK);
        } else {
            throw new ConfigurationException("AWS configuration invalid");
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
        filter.setInstanceId(initConfigurationService.getInstanceId());
        filterProxy.setFilter(filter);

        LOG.info("Context refreshed successfully.");
        CONTEXT_REFRESH_IN_PROCESS = false;
    }

    private static class ConfigDto {
        private User user;
        private String bucketName;
        private String volumeSize;

        public String getVolumeSize() {
            return volumeSize;
        }

        public void setVolumeSize(final String volumeSize) {
            this.volumeSize = volumeSize;
        }

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

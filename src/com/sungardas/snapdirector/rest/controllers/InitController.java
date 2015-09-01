package com.sungardas.snapdirector.rest.controllers;

import com.sungardas.snapdirector.aws.dynamodb.Roles;
import com.sungardas.snapdirector.aws.dynamodb.model.User;
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

import javax.servlet.Filter;
import java.net.URI;
import java.net.URISyntaxException;

import static org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR;


@RequestMapping("/session")
public class InitController implements ApplicationContextAware {

	@ExceptionHandler(SnapdirectorException.class)
	@ResponseBody
	@ResponseStatus(INTERNAL_SERVER_ERROR)
	private Exception internalServerError(SnapdirectorException exception){
		LOG.error(exception);
		return exception;
	}

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
	private boolean awsPropertyFileExists = false;


	private static boolean CONTEXT_REFRESH_IN_PROCESS = false;

	@RequestMapping(method = RequestMethod.POST)
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

	public void init() {
		awsPropertyFileExists = credentialsService.isAwsPropertyFileExists();
		if (awsPropertyFileExists && credentialsService.areCredentialsValid()) {
			refreshContext();
		}


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

	public static boolean isContextRefreshInProcess() {
		return CONTEXT_REFRESH_IN_PROCESS;
	}


}

package com.sungardas.snapdirector.rest.controllers;

import com.sungardas.snapdirector.aws.dynamodb.model.User;
import com.sungardas.snapdirector.rest.filters.FilterProxy;
import com.sungardas.snapdirector.rest.utils.Constants;
import com.sungardas.snapdirector.service.InitializationService;
import com.sungardas.snapdirector.service.impl.InitializationServiceImpl;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.support.XmlWebApplicationContext;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import javax.servlet.Filter;
import javax.servlet.http.HttpServletResponse;


@RequestMapping("/session")
public class InitController implements ApplicationContextAware {

	private static final Logger LOG = LogManager.getLogger(InitController.class);

	@Autowired
	private FilterProxy filterProxy;

	private InitializationService initializationService = new InitializationServiceImpl();

	@Autowired
	private ApplicationContext applicationContext;


	private static boolean CONTEXT_REFRESH_IN_PROCESS = false;

	@RequestMapping(method = RequestMethod.POST)
	public String init(@RequestBody User user, RedirectAttributes model, HttpServletResponse response) {
		if (CONTEXT_REFRESH_IN_PROCESS) {
			return HttpStatus.NO_CONTENT.toString();
		}
		if (initializationService.ValidAWSCredentialsAreProvided()) {
			LOG.info("Valid aws credentials were provided.");
			refreshContext();
			// update initialization service
			initializationService = (InitializationService) applicationContext.getBean("initializationServiceImpl");
			if (initializationService.isAdminUserExists()) {
				model.addFlashAttribute(Constants.JSON_AUTHENTIFICATION_EMAIL, user.getEmail());
				model.addFlashAttribute(Constants.JSON_AUTHENTIFICATION_PASSWORD, user.getPassword());
				return "redirect:/rest/session";
			}
		}
		try {
			if (initializationService.checkDefaultUser(user.getEmail(), user.getPassword())) {
				//TODO: should we redirect request somewhere or should we sent status OK and UI team will create request to conf controller from there side?
				return "redirect:/rest/configuration";
			}
		} catch (Exception e) {
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			return null;
		}
		response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
		return null;
	}

	public void init() {
		if (initializationService.ValidAWSCredentialsAreProvided()) {
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

package com.sungardas.snapdirector.rest;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sungardas.snapdirector.aws.PropertyBasedCredentialsProvider;
import com.sungardas.snapdirector.aws.dynamodb.DynamoUtils;
import com.sungardas.snapdirector.rest.utils.Constants;
import com.sungardas.snapdirector.rest.utils.JsonFromStream;
import com.sungardas.snapdirector.rest.utils.MultiReadHttpServletRequest;
import com.sungardas.snapdirector.service.DefaultUserAuthenticationService;
import com.sungardas.snapdirector.service.InitializationService;
import org.apache.catalina.connector.RequestFacade;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.FlashMap;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.sungardas.snapdirector.rest.utils.Constants.*;

@Service
public class RestAuthenticationFilter implements Filter {
	private static final Logger LOG = LogManager.getLogger(RestAuthenticationFilter.class);

	@Autowired
	private InitializationService initializationService;

	@Autowired
	private DefaultUserAuthenticationService defaultUserAuthenticationService;

	public void destroy() {
	}

	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws ServletException,
			IOException {
		Map<String, String> allowedSessions = (Map<String, String>) request.getServletContext().getAttribute(CONTEXT_ALLOWED_SESSIONS_ATR_NAME);
		if (allowedSessions == null) {
			request.getServletContext().setAttribute(CONTEXT_ALLOWED_SESSIONS_ATR_NAME, new HashMap<String, String>());
			allowedSessions = (Map<String, String>) request.getServletContext().getAttribute(CONTEXT_ALLOWED_SESSIONS_ATR_NAME);
		}
		if (request instanceof HttpServletRequest) {
			MultiReadHttpServletRequest multiReadRequest = new MultiReadHttpServletRequest((HttpServletRequest) request);
			HttpSession session = ((HttpServletRequest) request).getSession();

			boolean allowed = allowedSessions.containsKey(session.getId());
			if (!allowed && multiReadRequest.getPathInfo()!= null && multiReadRequest.getPathInfo().endsWith("/session")) {
				String email;
				String password;

				// temp solution: in case request was redirected from initController we need to get attributes from session
				List<FlashMap> attributes = ((List)((RequestFacade) request).getSession().
						getAttribute("org.springframework.web.servlet.support.SessionFlashMapManager.FLASH_MAPS"));
				if (attributes == null) {
					InputStream requestStream = multiReadRequest.getInputStream();
					JSONObject authCredentials = JsonFromStream.newJSONObject(requestStream);
					email = authCredentials.getString(JSON_AUTHENTIFICATION_EMAIL).toLowerCase();
					password = authCredentials.getString(JSON_AUTHENTIFICATION_PASSWORD);
				} else {
					email = ((String) attributes.get(0).get(Constants.JSON_AUTHENTIFICATION_EMAIL)).toLowerCase();
					password = (String) attributes.get(0).get(Constants.JSON_AUTHENTIFICATION_PASSWORD);
				}
				if (initializationService.isAdminUserExists()) {
					allowed = DynamoUtils.authenticateUser(email, password, getMapper());
				} else {
					allowed = defaultUserAuthenticationService.checkDefaultUser(email,password);
				}
				if (allowed) {
					allowedSessions.put(session.getId(), email);
					LOG.info("Add session to allowed list: [{}] [{}]", session.getId(), email);
				}
			}
			if (allowed) {
				try {
					chain.doFilter(multiReadRequest, response);
				} catch (IOException e) {
					e.printStackTrace();
				}
			} else {
				LOG.info("Authentication failed. Session [{}]", session.getId());
				if (response instanceof HttpServletResponse) {
					HttpServletResponse httpServletResponse = (HttpServletResponse) response;
					httpServletResponse.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
				}
			}
		}

	}

	public void init(FilterConfig fConfig) throws ServletException {
	}

	private DynamoDBMapper getMapper() {
		AmazonDynamoDBClient client = new AmazonDynamoDBClient(new PropertyBasedCredentialsProvider());
		return new DynamoDBMapper(client);
	}
}

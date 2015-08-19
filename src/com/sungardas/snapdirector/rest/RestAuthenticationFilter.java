package com.sungardas.snapdirector.rest;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sungardas.snapdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sungardas.snapdirector.aws.dynamodb.DynamoUtils;
import com.sungardas.snapdirector.rest.utils.JsonFromStream;
import com.sungardas.snapdirector.rest.utils.MultiReadHttpServletRequest;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import javax.servlet.*;
import javax.servlet.annotation.WebFilter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import static com.sungardas.snapdirector.rest.utils.Constants.*;

@WebFilter("/rest/*")
public class RestAuthenticationFilter implements Filter {
	private static final Logger LOG = LogManager.getLogger(RestAuthenticationFilter.class);

	public RestAuthenticationFilter() {
	}

	public void destroy() {
	}

	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws ServletException,
			IOException {

		Map<String, String> allowedSessions = (Map<String, String>) request.getServletContext().getAttribute(CONTEXT_ALLOWED_SESSIONS_ATR_NAME);
		if (allowedSessions == null) {
			request.getServletContext().setAttribute(CONTEXT_ALLOWED_SESSIONS_ATR_NAME, new HashMap<String, String>());
			allowedSessions = (Map<String, String>) request.getServletContext().getAttribute(CONTEXT_ALLOWED_SESSIONS_ATR_NAME);
		}

		LOG.info("RestAuthenticationFilter:doFilter");
		LOG.info("RestAuthenticationFilter:sessionid=" + ((HttpServletRequest) request).getSession().getId());
		if (request instanceof HttpServletRequest) {
			MultiReadHttpServletRequest multiReadRequest = new MultiReadHttpServletRequest((HttpServletRequest) request);
			HttpSession session = ((HttpServletRequest) request).getSession();

			boolean allowed = allowedSessions.containsKey(session.getId());
			if (!allowed && multiReadRequest.getPathInfo().endsWith("/session")) {
				InputStream requestStream = multiReadRequest.getInputStream();
				JSONObject authCredentials = JsonFromStream.newJSONObject(requestStream);
				String email = authCredentials.getString(JSON_AUTHENTIFICATION_EMAIL);
				allowed = DynamoUtils.authenticateUser(email,
						authCredentials.getString(JSON_AUTHENTIFICATION_PASSWORD), getMapper(request));
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

	private DynamoDBMapper getMapper(ServletRequest request) {
		AmazonDynamoDBClient client = new AmazonDynamoDBClient(new EnvironmentBasedCredentialsProvider());
		String region = request.getServletContext().getInitParameter("aws:dynamodb-region");
		return new DynamoDBMapper(client);
	}

}

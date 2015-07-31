package com.sangardas.snapshotdirector.rest;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.Set;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.annotation.WebFilter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.json.JSONObject;

import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sangardas.snapshotdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sangardas.snapshotdirector.aws.dynamodb.DynamoUtils;
import com.sangardas.snapshotdirector.rest.utils.JsonFromStream;
import com.sun.jersey.spi.container.ContainerRequest;
import com.sun.jersey.spi.container.ContainerResponse;
import com.sun.jersey.spi.container.ContainerResponseFilter;


/**
 * Servlet Filter implementation class RestAuthenticationFilter
 */

@WebFilter("/rest/*")
public class RestAuthenticationFilter implements Filter{
	private static final Log LOG = LogFactory.getLog(RestAuthenticationFilter.class);
	

    /**
     * Default constructor. 
     */
    public RestAuthenticationFilter() {
    }

	/**
	 * @see Filter#destroy()
	 */
	public void destroy() {
		// TODO Auto-generated method stub
	}

	/**
	 * @see Filter#doFilter(ServletRequest, ServletResponse, FilterChain)
	 */
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
		
		
		Set<String> allowedSessions = (Set<String>) request.getServletContext().getAttribute("allowedSessions");
		if (allowedSessions==null) {
			request.getServletContext().setAttribute("allowedSessions", new HashSet<String>());
			allowedSessions = (Set<String>) request.getServletContext().getAttribute("allowedSessions");
		}
		
		
		
		LOG.info("RestAuthenticationFilter:doFilter");
		LOG.info("RestAuthenticationFilter:sessionid=" + ((HttpServletRequest)request).getSession().getId());
		if (request instanceof HttpServletRequest) {
			HttpServletRequest httpServletRequest = (HttpServletRequest) request;
			
			LOG.info("content length: " + httpServletRequest.getContentLength());
			
			
					

			
			AuthenticationService authenticationService = new AuthenticationService();
			HttpSession session = ((HttpServletRequest) request).getSession();
			boolean allowed=false;
			
			allowed = allowedSessions.contains(session.getId());
			
			if(!allowed && httpServletRequest.getPathInfo().endsWith("/session")) {
				System.out.println(httpServletRequest.getPathInfo());
				LOG.info("RestAuthenticationFilter: new session " + session.getId());
				InputStream requestStream = httpServletRequest.getInputStream();
				JSONObject authCredentials = JsonFromStream.newJSONObject(requestStream);
				requestStream.close();
				LOG.info("email: " + authCredentials.getString("email"));
				LOG.info("password: " + authCredentials.getString("password"));
				
				allowed = DynamoUtils.authenticateUser(authCredentials.getString("email"), authCredentials.getString("password"), getMapper(request));
				allowedSessions.add(session.getId());
			}
			
			
			
			if (allowed) {
				LOG.info("RestAuthenticationFilter: alloved; session" + session.getId());
				chain.doFilter(request, response);
			} else {
				LOG.info("RestAuthenticationFilter: deny; session" + session.getId());
				if (response instanceof HttpServletResponse) {
					HttpServletResponse httpServletResponse = (HttpServletResponse) response;
					httpServletResponse
							.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
				}
			}
		}

	}

	/**
	 * @see Filter#init(FilterConfig)
	 */
	public void init(FilterConfig fConfig) throws ServletException {
		// TODO Auto-generated method stub
	}
	
	private DynamoDBMapper getMapper(ServletRequest request) {
		AmazonDynamoDBClient client = new AmazonDynamoDBClient(new EnvironmentBasedCredentialsProvider());
		String region = request.getServletContext().getInitParameter("aws:dynamodb-region");
		return  new DynamoDBMapper(client);
	}


}

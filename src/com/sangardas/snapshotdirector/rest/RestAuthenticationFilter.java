package com.sangardas.snapshotdirector.rest;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
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

import com.sangardas.snapshotdirector.rest.utils.JsonFromStream;


/**
 * Servlet Filter implementation class RestAuthenticationFilter
 */

@WebFilter("/rest/*")
public class RestAuthenticationFilter implements Filter {
	private static final Log LOG = LogFactory.getLog(RestAuthenticationFilter.class);
	private static final String AUTHENTICATION_HEADER = "Authorization";
	private HashMap<String, Boolean> sessions = new  HashMap<String, Boolean>();

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
			InputStream requestStream = httpServletRequest.getInputStream();
			LOG.info("content length: " + httpServletRequest.getContentLength());
			//TODO change to: JSONObject authCredentials = JsonFromStream.newJSONObject(requestStream);
			JSONObject authCredentials = null;
			requestStream.close();
					

			
			AuthenticationService authenticationService = new AuthenticationService();
			HttpSession session = ((HttpServletRequest) request).getSession();
			boolean allowed=false;
			if(session.isNew()) {
				LOG.info("RestAuthenticationFilter: new session " + session.getId());
				allowed = authenticationService.authenticateByCred(authCredentials);
				//Set<String> allowedSessions = (Set<String>) session.getServletContext().getAttribute("allowedSessions");
				allowedSessions.add(session.getId());
				LOG.info("RestAuthenticationFilter: allowed session" + session.getId());
			}
			else {
				//Set<String> allowedSessions = (Set<String>) session.getServletContext().getAttribute("allowedSessions");
				allowed = allowedSessions.contains(session.getId());
				LOG.info("RestAuthenticationFilter: session" + session.getId() + "allowed=" + allowed);
			}
			
			if (allowed) {
				LOG.info("RestAuthenticationFilter: alloved");
				chain.doFilter(request, response);
			} else {
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

}

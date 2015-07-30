package com.sangardas.snapshotdirector.rest;

import java.io.IOException;
import java.util.HashMap;

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
        // TODO Auto-generated constructor stub
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
		LOG.info("RestAuthenticationFilter:doFilter");
		LOG.info("RestAuthenticationFilter:sessionid=" + ((HttpServletRequest)request).getSession().getId());
		if (request instanceof HttpServletRequest) {
			HttpServletRequest httpServletRequest = (HttpServletRequest) request;
			String authCredentials = httpServletRequest
					.getHeader(AUTHENTICATION_HEADER);

			
			AuthenticationService authenticationService = new AuthenticationService();
			HttpSession session = ((HttpServletRequest) request).getSession();
			boolean alloved = authenticationService.authenticateByCred(authCredentials,session);
			if(!alloved) alloved = authenticationService.authenticateBySessionIs(session);

			if (alloved) {
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

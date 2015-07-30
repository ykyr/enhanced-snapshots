package com.sangardas.snapshotdirector.rest;

import java.util.Set;

import javax.servlet.annotation.WebListener;
import javax.servlet.http.HttpSessionBindingEvent;
import javax.servlet.http.HttpSessionBindingListener;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Application Lifecycle Listener implementation class SessionAuthentification
 *
 */
@WebListener
public class SessionAuthentification implements HttpSessionListener {
	private static final Log LOG = LogFactory.getLog(SessionAuthentification.class);

	/**
	 * Default constructor.
	 */
	public SessionAuthentification() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @see HttpSessionListener#sessionCreated(HttpSessionEvent)
	 */
	public void sessionCreated(HttpSessionEvent se) {
	}

	/**
	 * @see HttpSessionListener#sessionDestroyed(HttpSessionEvent)
	 */
	public void sessionDestroyed(HttpSessionEvent se) {

		String sessionId = se.getSession().getId();
		Set<String> allowedSessions = (Set<String>) se.getSession()
				.getServletContext().getAttribute("allowedSessions");
		allowedSessions.remove(sessionId);
		LOG.info("SessionAuthentification:sessionDestroyed: " + sessionId);
	}

}

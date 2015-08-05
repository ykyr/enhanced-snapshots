package com.sungardas.snapdirector.rest;

import static com.sungardas.snapdirector.rest.utils.Constants.CONTEXT_ALLOWED_SESSIONS_ATR_NAME;

import java.util.Set;

import javax.servlet.annotation.WebListener;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

@WebListener
public class SessionAuthentificationListener implements HttpSessionListener {
	private static final Log LOG = LogFactory.getLog(SessionAuthentificationListener.class);

	public SessionAuthentificationListener() {
	}

	public void sessionCreated(HttpSessionEvent se) {
	}

	@SuppressWarnings("unchecked")
	public void sessionDestroyed(HttpSessionEvent se) {
		String sessionId = se.getSession().getId();
		Set<String> allowedSessions = (Set<String>) se.getSession().getServletContext().getAttribute(CONTEXT_ALLOWED_SESSIONS_ATR_NAME);
		allowedSessions.remove(sessionId);
		LOG.info("sessionDestroyed: " + sessionId);
	}

}

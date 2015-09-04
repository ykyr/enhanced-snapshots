package com.sungardas.snapdirector.rest.filters;

import com.sungardas.snapdirector.aws.dynamodb.repository.UserRepository;
import com.sungardas.snapdirector.rest.utils.Constants;
import com.sungardas.snapdirector.rest.utils.JsonFromStream;
import com.sungardas.snapdirector.rest.utils.MultiReadHttpServletRequest;
import org.apache.catalina.connector.RequestFacade;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.FlashMap;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.sungardas.snapdirector.rest.utils.Constants.*;

@Service
public class RestAuthenticationFilter implements com.sungardas.snapdirector.rest.RestAuthenticationFilter {

    private static final Logger LOG = LogManager.getLogger(RestAuthenticationFilter.class);

    private UserRepository userRepository;

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
            if (!allowed && multiReadRequest.getPathInfo().endsWith("/session")) {
                InputStream requestStream = multiReadRequest.getInputStream();
                JSONObject authCredentials = JsonFromStream.newJSONObject(requestStream);
                String email = authCredentials.getString(JSON_AUTHENTIFICATION_EMAIL).toLowerCase();
                String password = authCredentials.getString(JSON_AUTHENTIFICATION_PASSWORD);
                allowed = !userRepository.findByEmailAndPassword(email, DigestUtils.sha512Hex(password)).isEmpty();
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

    @Override
    public void setUserRepository(UserRepository userRepository) {
        this.userRepository = userRepository;
    }
}

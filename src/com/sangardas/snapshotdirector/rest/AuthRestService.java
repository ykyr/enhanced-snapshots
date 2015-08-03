package com.sangardas.snapshotdirector.rest;

import static com.sangardas.snapshotdirector.rest.utils.Constants.*;

import java.io.IOException;
import java.io.InputStream;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DELETE;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

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
import com.sangardas.snapshotdirector.rest.utils.MultiReadHttpServletRequest;

@Path("/session")
public class AuthRestService {
	private static final Log LOG = LogFactory.getLog(AuthRestService.class);

	@Context
	ServletContext context;
	@Context
	private HttpServletRequest servletRequest;

	@POST()
	@Produces(MediaType.APPLICATION_JSON)
	public String login() {
		MultiReadHttpServletRequest multiReadRequest = new MultiReadHttpServletRequest(
				(HttpServletRequest) servletRequest);
		String result = null;
		try (InputStream requestStream = multiReadRequest.getInputStream()) {
			System.out.println("available:" + requestStream.available());
			JSONObject authCredentials = JsonFromStream.newJSONObject(requestStream);
			result = DynamoUtils.getFullUserInfoByEmail(authCredentials.getString(JSON_AUTHENTIFICATION_EMAIL), getMapper(servletRequest));
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		return result;
	}

	private DynamoDBMapper getMapper(ServletRequest request) {
		AmazonDynamoDBClient client = new AmazonDynamoDBClient(new EnvironmentBasedCredentialsProvider());
		String region = request.getServletContext().getInitParameter("aws:dynamodb-region");
		client.setRegion(Region.getRegion(Regions.fromName(region)));
		return new DynamoDBMapper(client);
	}

	@DELETE()
	@Produces(MediaType.APPLICATION_JSON)
	public String logout(@Context HttpServletRequest request) {

		String sessionId = request.getSession().getId();
		Set<String> allowedSessions = (Set<String>) context.getAttribute(CONTEXT_ALLOWED_SESSIONS_ATR_NAME);
		boolean result = allowedSessions.remove(sessionId);
		LOG.info("Logout for session: " + sessionId);
		return String.valueOf(result);

	}

	@Path("/{sessionIdFromClient}")
	@DELETE()
	@Produces(MediaType.APPLICATION_XML)
	public String logoutXML(@Context HttpServletRequest request,
			@PathParam("sessionIdFromClient") String sessionIdClient) {

		String sessionId = request.getSession().getId();
		Set<String> allowedSessions = (Set<String>) context.getAttribute(CONTEXT_ALLOWED_SESSIONS_ATR_NAME);
		boolean result = allowedSessions.remove(sessionId);
		LOG.info("Logout for session: " + sessionId);
		return String.valueOf(result);

	}
}

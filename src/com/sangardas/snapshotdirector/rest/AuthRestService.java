package com.sangardas.snapshotdirector.rest;

import javax.servlet.ServletContext;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

import org.json.JSONObject;

import com.sangardas.snapshotdirector.rest.utils.JsonFromFile;


@Path("/user")
public class AuthRestService {

	@Context
	ServletContext context;


	@GET()
	@Produces(MediaType.APPLICATION_JSON)
	public String get() {
		String path = context.getInitParameter("rest:mock-directory");
		String result = null;
		try {
			JSONObject user = JsonFromFile.newJSONObject(path + "user.json");
			result = user.toString();
		} catch (Exception e) {
			throw new WebApplicationException(e);
		}
		return result;

	}
}

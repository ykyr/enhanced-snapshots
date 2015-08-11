package com.sungardas.snapdirector.rest;

import javax.servlet.ServletContext;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

import org.json.JSONArray;

import com.sungardas.snapdirector.rest.utils.JsonFromFile;


@Path("/schedule")
public class ScheduleRestService {

	@Context
	ServletContext context;


	@GET()
	@Produces(MediaType.APPLICATION_JSON)
	public String get() {
			String path = context.getInitParameter("rest:mock-directory");
			String result = null;
			try {
				JSONArray schedule = JsonFromFile.newJSONArray(path + "schedules.json");
				result = schedule.toString();
			} catch (Exception e) {
				throw new WebApplicationException(e);
			}
			return result;

	}

}

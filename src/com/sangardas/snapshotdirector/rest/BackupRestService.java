package com.sangardas.snapshotdirector.rest;

import javax.servlet.ServletContext;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

import org.json.JSONArray;

import com.sangardas.snapshotdirector.rest.utils.JsonFromFile;


@Path("/backup")
public class BackupRestService {

	@Context
	ServletContext context;


	@GET()
	@Produces(MediaType.APPLICATION_JSON)
	public String get() {
		String path = context.getInitParameter("rest:mock-directory");
		String result = null;
		try {
			JSONArray backups = JsonFromFile.newJSONArray(path + "backups.json");
			result = backups.toString();
		} catch (Exception e) {
			throw new WebApplicationException(e);
		}
		return result;
	}

}

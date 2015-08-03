package com.sangardas.snapshotdirector.rest;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

import org.json.JSONArray;
import org.json.JSONObject;

import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sangardas.snapshotdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sangardas.snapshotdirector.aws.dynamodb.DynamoUtils;
import com.sangardas.snapshotdirector.aws.dynamodb.model.BackupEntry;
import com.sangardas.snapshotdirector.rest.utils.JsonFromFile;
import com.sangardas.snapshotdirector.rest.utils.JsonFromStream;
import com.sangardas.snapshotdirector.rest.utils.MultiReadHttpServletRequest;


@Path("/backup")
public class BackupRestService {

	@Context
	ServletContext context;
	@Context
	private HttpServletRequest servletRequest;
	


	@GET()
	@Path("/{volumeId}")
	@Produces(MediaType.APPLICATION_JSON)
	public String get(@PathParam("volumeId") String volumeId) {
		
		List<BackupEntry> items = DynamoUtils.getBackupInfo(volumeId, getMapper(servletRequest));
		JSONArray result = new JSONArray();
		for(BackupEntry backup: items) {
			result.put(backup.toString());
		}
		System.out.println(result.toString());
		return result.toString();
	}
	
	@DELETE()
	@Produces(MediaType.APPLICATION_JSON)
	@Path("/{backupfileName}")
	public String deleteBackup(@PathParam("backupfileName") String filename) {
		
		return null;
	}
	
	private DynamoDBMapper getMapper(ServletRequest request) {
		AmazonDynamoDBClient client = new AmazonDynamoDBClient(new EnvironmentBasedCredentialsProvider());
		String region = request.getServletContext().getInitParameter("aws:dynamodb-region");
		client.setRegion(Region.getRegion(Regions.fromName(region)));
		return new DynamoDBMapper(client);
	}

}

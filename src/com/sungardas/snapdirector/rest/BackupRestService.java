package com.sungardas.snapdirector.rest;

import static com.sungardas.snapdirector.rest.utils.Constants.JSON_VOLUME_AVAILABILITY_ZONE;
import static com.sungardas.snapdirector.rest.utils.Constants.JSON_VOLUME_CREATE_TIME;
import static com.sungardas.snapdirector.rest.utils.Constants.JSON_VOLUME_SIZE;
import static com.sungardas.snapdirector.rest.utils.Constants.JSON_VOLUME_VOLUME_ID;

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
import javax.ws.rs.core.Response;

import org.json.JSONArray;
import org.json.JSONObject;

import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.ec2.model.Volume;
import com.sungardas.snapdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sungardas.snapdirector.aws.dynamodb.DynamoUtils;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.rest.utils.JsonFromFile;
import com.sungardas.snapdirector.rest.utils.JsonFromStream;
import com.sungardas.snapdirector.rest.utils.MultiReadHttpServletRequest;


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
		
		return jsonArrayRepresentation(items).toString();
	}
	
	private JSONArray jsonArrayRepresentation(List<BackupEntry> backupEntries) {
		JSONArray backupsJSONArray = new JSONArray();
		for(BackupEntry entry: backupEntries) {
			JSONObject backupItem = new JSONObject();
			backupItem.put("message", entry.getMessage());
			backupItem.put("fileName", entry.getFileName());
			backupItem.put("volumeId", entry.getVolumeId());
			backupItem.put("timeCreated", entry.getTimeCreated());
			backupsJSONArray.put(backupItem);
		}

		return backupsJSONArray;
	}
	
	@DELETE()
	@Produces(MediaType.APPLICATION_JSON)
	@Path("/{backupfileName}")
	public Response deleteBackup(@PathParam("backupfileName") String filename) {
		String volumeId = filename.substring(0, 12);
		System.out.println(volumeId);
		boolean statusOk = DynamoUtils.removeBackupInfo(volumeId, filename, getMapper(servletRequest));
		
		 if(statusOk) {
			 return Response.ok().build();
			 }
		 else return Response.serverError().build();
		
	}
	
	private DynamoDBMapper getMapper(ServletRequest request) {
		AmazonDynamoDBClient client = new AmazonDynamoDBClient(new EnvironmentBasedCredentialsProvider());
		String region = request.getServletContext().getInitParameter("aws:dynamodb-region");
		client.setRegion(Region.getRegion(Regions.fromName(region)));
		return new DynamoDBMapper(client);
	}

}

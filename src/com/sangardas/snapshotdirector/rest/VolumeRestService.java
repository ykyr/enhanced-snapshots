package com.sangardas.snapshotdirector.rest;

import java.util.List;

import javax.servlet.ServletContext;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

import org.json.JSONArray;
import org.json.JSONObject;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.AmazonEC2Client;
import com.amazonaws.services.ec2.model.Volume;
import com.sangardas.snapshotdirector.aws.PropertiesResourceFileCredentialsProvider;
import com.sangardas.snapshotdirector.aws.S3Utils;
import com.sangardas.snapshotdirector.rest.utils.JsonFromFile;


@Path("/volume")
public class VolumeRestService {

	@Context
	ServletContext context;


	@Produces(MediaType.APPLICATION_JSON)
	@GET()
	public String get() {
		String result = null;
		try {
			String path = context.getInitParameter("aws:credentials-file");
			AmazonEC2 ec2Client = new AmazonEC2Client(new PropertiesResourceFileCredentialsProvider(path));
			Region usEast = Region.getRegion(Regions.US_EAST_1);
			ec2Client.setRegion(usEast);

			List<Volume> volumes = S3Utils.getVolumeList(ec2Client);
			JSONArray volumesJSONArray = new JSONArray();
			for (Volume v : volumes) {
				JSONObject volumeJSONObject = new JSONObject();
				volumeJSONObject.put("ID", v.getVolumeId());
				volumeJSONObject.put("Capacity", v.getSize());
				volumeJSONObject.put("Created", v.getCreateTime());
				volumeJSONObject.put("Zone", v.getAvailabilityZone());
				volumesJSONArray.put(volumeJSONObject);
			}

			result = volumesJSONArray.toString();
		} catch (Exception e) {
			throw new WebApplicationException(e);
		}
		return result;
	}

}

package com.sungardas.snapdirector.rest;

import static com.sungardas.snapdirector.rest.utils.Constants.*;

import java.util.LinkedList;
import java.util.List;

import javax.servlet.ServletContext;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.json.JSONArray;
import org.json.JSONObject;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.AmazonEC2Client;
import com.amazonaws.services.ec2.model.Volume;
import com.sungardas.snapdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sungardas.snapdirector.aws.S3Utils;

@Path("/volume")
public class VolumeRestService {
	private static final Log LOG = LogFactory.getLog(VolumeRestService.class);

	@Context
	ServletContext context;

	@Produces(MediaType.APPLICATION_JSON)
	@GET()
	public String getAllVolumes() {
		String result = null;
		try {
			AmazonEC2 ec2Client = new AmazonEC2Client(new EnvironmentBasedCredentialsProvider());
			List<Volume> volumes = new LinkedList<Volume>();

			for (Regions nextRegion : Regions.values()) {
				try {
					ec2Client.setRegion(Region.getRegion(nextRegion));
					//for(int i=0;i<20;i++)
						volumes.addAll(S3Utils.getVolumeList(ec2Client));
					
					
				} catch (AmazonServiceException unreachableRegion) {
					continue;
				}
			}
			result = jsonArrayRepresentation(volumes).toString();
		} catch (Exception e) {
			throw new WebApplicationException(e);
		}

		LOG.info("get all volumes: " + result);
		return result;
	}

	@Path("/{regionId}")
	@Produces(MediaType.APPLICATION_JSON)
	@GET()
	public String get(@PathParam("regionId") String region) {
		String result = null;
		try {
			AmazonEC2 ec2Client = new AmazonEC2Client(new EnvironmentBasedCredentialsProvider());
			Region usEast = Region.getRegion(Regions.valueOf(region));
			ec2Client.setRegion(usEast);

			List<Volume> volumes = S3Utils.getVolumeList(ec2Client);
			result = jsonArrayRepresentation(volumes).toString();
		} catch (Exception e) {
			throw new WebApplicationException(e);
		}
		LOG.info("get  volumes for region " + region + ": " + result);

		return result;
	}

	private JSONArray jsonArrayRepresentation(List<Volume> volumes) {
		JSONArray volumesJSONArray = new JSONArray();
		for (Volume v : volumes) {
			JSONObject volumeJSONObject = new JSONObject();
			volumeJSONObject.put(JSON_VOLUME_VOLUME_NAME, "");
			volumeJSONObject.put(JSON_VOLUME_VOLUME_ID, v.getVolumeId());
			volumeJSONObject.put(JSON_VOLUME_SIZE, v.getSize());
			volumeJSONObject.put(JSON_VOLUME_VOLUME_SNAPSHOT_ID, v.getSnapshotId());
			volumeJSONObject.put(JSON_VOLUME_CREATE_TIME, v.getCreateTime().getTime());
			volumeJSONObject.put(JSON_VOLUME_AVAILABILITY_ZONE, v.getAvailabilityZone());
			volumeJSONObject.put(JSON_VOLUME_STATE, v.getState());
			volumesJSONArray.put(volumeJSONObject);
		}

		return volumesJSONArray;
	}

}

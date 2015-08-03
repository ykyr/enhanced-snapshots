package com.sangardas.snapshotdirector.rest;

import javax.ws.rs.GET;
import javax.ws.rs.Path;

import org.json.JSONArray;

import com.amazonaws.regions.Regions;

@Path("/regions")
public class RegionsRestService {
	
	@GET()
	public String getRegion() {
		JSONArray regionsJson = new JSONArray();
		for(Regions nextRegion: Regions.values())
			regionsJson.put(nextRegion.getName());
		return regionsJson.toString();
		
	}

}

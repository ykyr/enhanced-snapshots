package com.sungardas.snapdirector.rest;

import javax.ws.rs.GET;
import javax.ws.rs.Path;

import org.json.JSONArray;
import org.json.JSONObject;

import com.amazonaws.regions.Regions;

@Path("/regions")
public class RegionsRestService {
	
	@GET()
	public String getRegions() {
		JSONObject record=null;
		JSONArray regionsJson = new JSONArray();
		for(Regions nextRegion: Regions.values()){
			record = new JSONObject();
			record.put("id", nextRegion.getName());
			record.put("name", nextRegion.getName());
			regionsJson.put(record);
		}
		return regionsJson.toString();
		
	}

}

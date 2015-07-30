package com.sangardas.snapshotdirector.rest;

import java.util.HashSet;

import javax.servlet.http.HttpSession;

import org.json.JSONObject;

public class AuthenticationService {
	
	
	public boolean authenticateByCred(JSONObject authCredentials) {
		authCredentials.get("email");
		authCredentials.get("password");

		return true;
	}
	
	public JSONObject getUser(String key) {
		return new JSONObject("{fullname: \"sungard user\"}");
	}
	

	
	


}

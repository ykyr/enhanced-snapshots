package com.sangardas.snapshotdirector.rest;

import java.util.HashSet;

import javax.servlet.http.HttpSession;

public class AuthenticationService {
	private static HashSet<String> allowed = new HashSet<String>();
	
	
	public boolean authenticateByCred(String authCredentials, HttpSession session) {
		String sessionId= session.getId();
		if(true) allowed.add(sessionId);
		return true;
	}
	public boolean authenticateBySessionIs(HttpSession session) {
		String sessionId= session.getId();
		if(allowed.contains(sessionId)) {
			return true;
		}
		return false;
	}
	

	
	


}

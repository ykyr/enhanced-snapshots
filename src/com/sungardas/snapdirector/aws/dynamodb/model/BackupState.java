package com.sungardas.snapdirector.aws.dynamodb.model;

public enum BackupState {
	INPROGRESS("inProgress"), COMPLETED("completed"), DELETING("deleting"), FAILED("failed");
	
	private final String state;
	
	BackupState(String state){
		this.state = state;
	}
	
	public String getState(){
		return this.state;
	}
}

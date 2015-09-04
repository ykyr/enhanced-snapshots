package com.sungardas.snapdirector.aws.dynamodb;


public enum Roles {

	ADMIN("admin"),
	USER("user"),
	CONFIGURATOR("configurator");

	private final String name;

	Roles(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}

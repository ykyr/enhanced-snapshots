package com.sangardas.snapshotdirector.tasks.aws.sdfs.utils;

public class IllegalSdfsStateException extends RuntimeException {

	private static final long serialVersionUID = 2565125974551021859L;


	public IllegalSdfsStateException() {
		super();
	}


	public IllegalSdfsStateException(String message) {
		super(message);
	}


	public IllegalSdfsStateException(String message, Throwable cause) {
		super(message, cause);
	}


	public IllegalSdfsStateException(Throwable cause) {
		super(cause);
	}


	protected IllegalSdfsStateException(String message, Throwable cause, boolean enableSuppression,
			boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

}

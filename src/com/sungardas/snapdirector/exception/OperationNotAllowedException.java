package com.sungardas.snapdirector.exception;


public class OperationNotAllowedException extends SnapdirectorException {
	public OperationNotAllowedException() {
	}

	public OperationNotAllowedException(String message) {
		super(message);
	}

	public OperationNotAllowedException(String message, Throwable cause) {
		super(message, cause);
	}

	public OperationNotAllowedException(Throwable cause) {
		super(cause);
	}

	public OperationNotAllowedException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}

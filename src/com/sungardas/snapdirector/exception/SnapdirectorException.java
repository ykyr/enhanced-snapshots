package com.sungardas.snapdirector.exception;

/**
 * Created by iradaik on 8/17/2015.
 */
public class SnapdirectorException extends RuntimeException {
    public SnapdirectorException() {
    }

    public SnapdirectorException(String message) {
        super(message);
    }

    public SnapdirectorException(String message, Throwable cause) {
        super(message, cause);
    }

    public SnapdirectorException(Throwable cause) {
        super(cause);
    }

    public SnapdirectorException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}

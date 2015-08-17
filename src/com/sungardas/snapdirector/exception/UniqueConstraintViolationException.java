package com.sungardas.snapdirector.exception;


public class UniqueConstraintViolationException extends SnapdirectorException {
    public UniqueConstraintViolationException() {
    }

    public UniqueConstraintViolationException(String message) {
        super(message);
    }

    public UniqueConstraintViolationException(String message, Throwable cause) {
        super(message, cause);
    }

    public UniqueConstraintViolationException(Throwable cause) {
        super(cause);
    }

    public UniqueConstraintViolationException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}

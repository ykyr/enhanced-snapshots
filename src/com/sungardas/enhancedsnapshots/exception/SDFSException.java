package com.sungardas.enhancedsnapshots.exception;


public class SDFSException extends EnhancedSnapshotsException {
    public SDFSException() {
    }

    public SDFSException(String message) {
        super(message);
    }

    public SDFSException(String message, Throwable cause) {
        super(message, cause);
    }

    public SDFSException(Throwable cause) {
        super(cause);
    }

    public SDFSException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}

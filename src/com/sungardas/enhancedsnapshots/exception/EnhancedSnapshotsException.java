package com.sungardas.enhancedsnapshots.exception;

public class EnhancedSnapshotsException extends RuntimeException {
    public EnhancedSnapshotsException() {
    }

    public EnhancedSnapshotsException(String message) {
        super(message);
    }

    public EnhancedSnapshotsException(String message, Throwable cause) {
        super(message, cause);
    }

    public EnhancedSnapshotsException(Throwable cause) {
        super(cause);
    }

    public EnhancedSnapshotsException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}

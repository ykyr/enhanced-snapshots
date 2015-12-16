package com.sungardas.enhancedsnapshots.exception;

public class EnhancedSnapshotsInterruptedException extends EnhancedSnapshotsException {
    public EnhancedSnapshotsInterruptedException() {
    }

    public EnhancedSnapshotsInterruptedException(String message) {
        super(message);
    }

    public EnhancedSnapshotsInterruptedException(String message, Throwable cause) {
        super(message, cause);
    }

    public EnhancedSnapshotsInterruptedException(Throwable cause) {
        super(cause);
    }

    public EnhancedSnapshotsInterruptedException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}

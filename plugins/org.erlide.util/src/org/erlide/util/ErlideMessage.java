package org.erlide.util;

public class ErlideMessage {

    private final String details;
    private final String message;
    private final int severity;

    public ErlideMessage(final int severity, final String message, final String details) {
        this.severity = severity;
        this.message = message;
        this.details = details;
    }

    public int getSeverity() {
        return severity;
    }

    public String getMessage() {
        return message;
    }

    public String getDetails() {
        return details;
    }

}

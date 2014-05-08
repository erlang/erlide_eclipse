package org.erlide.util;

public abstract class MessageReporter {

    /**
     * Status type severity (bit mask, value 1) indicating this status is
     * informational only.
     *
     * @see #getSeverity()
     * @see #matches(int)
     */
    public static final int INFO = 0x01;

    /**
     * Status type severity (bit mask, value 2) indicating this status
     * represents a warning.
     *
     * @see #getSeverity()
     * @see #matches(int)
     */
    public static final int WARNING = 0x02;

    /**
     * Status type severity (bit mask, value 4) indicating this status
     * represents an error.
     *
     * @see #getSeverity()
     * @see #matches(int)
     */
    public static final int ERROR = 0x04;

    abstract public void displayMessage(int severity, String message, String details);

    public static void showError(final String message) {
        show(ERROR, message, null);
    }

    public static void showWarning(final String message) {
        show(WARNING, message, null);
    }

    public static void showInfo(final String message) {
        show(INFO, message, null);
    }

    public static void showError(final String message, final String details) {
        show(ERROR, message, details);
    }

    public static void showWarning(final String message, final String details) {
        show(WARNING, message, details);
    }

    public static void showInfo(final String message, final String details) {
        show(INFO, message, details);
    }

    public static void show(final int severity, final String message, final String details) {
        ErlideEventBus.post(new ErlideMessage(severity, message, details));
    }

}

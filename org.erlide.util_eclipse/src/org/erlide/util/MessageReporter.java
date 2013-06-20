package org.erlide.util;

import org.eclipse.core.runtime.IStatus;

public abstract class MessageReporter {

    abstract public void displayMessage(int severity, String message,
            String details);

    public static void showError(final String message) {
        show(IStatus.ERROR, message, null);
    }

    public static void showWarning(final String message) {
        show(IStatus.WARNING, message, null);
    }

    public static void showInfo(final String message) {
        show(IStatus.INFO, message, null);
    }

    public static void showError(final String message, final String details) {
        show(IStatus.ERROR, message, details);
    }

    public static void showWarning(final String message, final String details) {
        show(IStatus.WARNING, message, details);
    }

    public static void showInfo(final String message, final String details) {
        show(IStatus.INFO, message, details);
    }

    public static void show(final int severity, final String message,
            final String details) {
        ErlideEventBus.post(new ErlideMessage(severity, message, details));
        ErlLogger.info(severity + "::: " + message + "\n" + details + "\n------");
    }

}

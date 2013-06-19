package org.erlide.util;

import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.RegistryFactory;
import org.erlide.util.services.ExtensionUtils;

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

    public static void show(final int type, final String message,
            final String details) {
        final List<MessageReporter> reporters = getAllImplementors();
        for (final MessageReporter reporter : reporters) {
            reporter.displayMessage(type, message, details);
        }
        ErlLogger.info(type + "::: " + message + "\n" + details + "\n------");
    }

    private static List<MessageReporter> getAllImplementors() {
        final List<MessageReporter> result = ExtensionUtils.getExtensions(
                "org.erlide.util_eclipse.messageReporter",
                MessageReporter.class);
        return result;
    }

    public static IConfigurationElement[] getMessageReporterConfigurationElements() {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        return reg.getConfigurationElementsFor("org.erlide.util_eclipse",
                "messageReporter");
    }

}

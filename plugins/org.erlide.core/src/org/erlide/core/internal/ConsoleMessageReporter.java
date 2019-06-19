package org.erlide.core.internal;

import org.eclipse.core.runtime.Plugin;
import org.erlide.util.ErlLogger;
import org.erlide.util.ErlideMessage;
import org.osgi.framework.Bundle;

import com.google.common.eventbus.Subscribe;

public class ConsoleMessageReporter {

    private final Plugin plugin;

    public ConsoleMessageReporter(final Plugin plugin) {
        this.plugin = plugin;
    }

    @Subscribe
    public void displayMessage(final ErlideMessage emsg) {
        if (plugin.getBundle().getState() != Bundle.STOPPING) {
            ErlLogger.info(emsg.getSeverity() + "::: " + emsg.getMessage() + "\n"
                    + emsg.getDetails() + "\n------");
        }
    }
}

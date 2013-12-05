package org.erlide.core;

import org.erlide.util.ErlLogger;
import org.erlide.util.ErlideMessage;

import com.google.common.eventbus.Subscribe;

public class ConsoleMessageReporter {

    @Subscribe
    public void displayMessage(final ErlideMessage emsg) {
        if (!ErlangPlugin.getDefault().isStopping()) {
            ErlLogger.info(emsg.getSeverity() + "::: " + emsg.getMessage() + "\n"
                    + emsg.getDetails() + "\n------");
        }
    }
}

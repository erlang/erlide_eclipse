package org.erlide.runtime.internal;

import org.erlide.runtime.api.ErlSystemStatus;
import org.erlide.util.ErlLogger;
import org.erlide.util.MessageReporter;
import org.erlide.util.SystemConfiguration;

public class ErlRuntimeReporter {

    private final boolean internal;

    public ErlRuntimeReporter(final boolean internal) {
        this.internal = internal;
    }

    public String reportRuntimeDown(final String peer,
            final ErlSystemStatus status) {
        final String fmt = "Backend '%s' is down";
        String msg = String.format(fmt, peer);
        // TODO when to report errors?
        final boolean shouldReport = internal;
        if (shouldReport) {
            final String user = System.getProperty("user.name");

            String msg1;
            if (internal) {
                msg1 = "If this happened at startup, it is likely that your network is misconfigured or uses 'strange' host names.\n\n"
                        + "Please check the page"
                        + "Window->preferences->erlang->network for hints about that.\n\n"
                        + "Also, check if you can create and connect two erlang nodes on your machine "
                        + "using \"erl -name foo1\" and \"erl -name foo2\".\n\n"
                        + "If this happened after a while, the Erlang runtime might have run out of memory.";
                msg += "\n\nPlease report the problem and restart Eclipse.";
            } else {
                msg1 = "If you didn't shut it down on purpose, it is an "
                        + "unrecoverable error, please restart Eclipse. ";
            }

            final String details = "If an error report named '"
                    + user
                    + "_<timestamp>.txt' has been created in your home directory, "
                    + "please consider reporting the problem. \n"
                    + (SystemConfiguration
                            .hasFeatureEnabled("erlide.ericsson.user") ? ""
                            : "http://www.assembla.com/spaces/erlide/support/tickets");
            MessageReporter.showError(msg, msg1 + "\n\n" + details);
        }
        ErlLogger.error("Last system status was:\n %s",
                status != null ? status.prettyPrint() : "null");
        return msg;
    }
}

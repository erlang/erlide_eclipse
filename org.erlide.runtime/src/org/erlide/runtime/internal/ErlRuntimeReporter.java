package org.erlide.runtime.internal;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;

import org.erlide.util.ErlLogger;
import org.erlide.util.LogUtil;
import org.erlide.util.MessageReporter;
import org.erlide.util.SystemConfiguration;
import org.erlide.util.event_tracer.ErlideEventTracer;

public class ErlRuntimeReporter {

    private final boolean internal;

    public ErlRuntimeReporter(final boolean internal) {
        this.internal = internal;
    }

    public String reportRuntimeDown(final String peer) {
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
                    + (SystemConfiguration.hasFeatureEnabled("erlide.ericsson.user") ? ""
                            : "http://www.assembla.com/spaces/erlide/support/tickets");
            MessageReporter.showError(msg, msg1 + "\n\n" + details);
        }
        return msg;
    }

    FilenameFilter filter = new FilenameFilter() {
        @Override
        public boolean accept(final File dir, final String name) {
            return name.matches("^core.[0-9]+$");
        }
    };

    public void createFileReport(final String nodeName, final int exitCode) {
        final String msg = String.format("Backend '%s' crashed with exit code %d.",
                nodeName, exitCode);

        String report = null;
        if (shouldCreateReport(exitCode)) {
            ErlLogger.error(msg);
            ErlideEventTracer.getInstance().traceCrash(nodeName);

            report = createReport(nodeName, exitCode, msg);
            final String reportMsg = report != null ? "\n\n"
                    + "An error log has been created at "
                    + report
                    + ". Please report the problem so that we can fix it.\n"
                    + (SystemConfiguration.hasFeatureEnabled("erlide.ericsson.user") ? ""
                            : "http://www.assembla.com/spaces/erlide/support/tickets")
                    : "";
            MessageReporter
                    .showError(
                            msg
                                    + "\n\n"
                                    + "This error is not recoverable, please restart your Eclipse instance.",
                            reportMsg);

        } else {
            ErlLogger.info(msg);
        }
    }

    private boolean shouldCreateReport(final int v) {
        // 129 = SIGHUP (probably logout, ignore)
        // 143 = SIGTERM (probably logout, ignore)
        // 137 = SIGKILL (probably killed by user)
        return v > 0 && v != 143 && v != 129 && v != 137;
    }

    private String createReport(final String nodeName, final int v, final String msg) {
        final String plog = LogUtil.fetchPlatformLog();
        final String elog = LogUtil.fetchErlideLog();
        final String delim = "\n==================================\n";
        final String reportFile = LogUtil.getReportFile();
        final File report = new File(reportFile);
        try {
            report.createNewFile();
            final OutputStream out = new FileOutputStream(report);
            final PrintWriter pw = new PrintWriter(out);
            try {
                pw.println(String.format(msg, nodeName, v));
                pw.println(System.getProperty("user.name"));
                pw.println(delim);
                pw.println(plog);
                pw.println(delim);
                pw.println(elog);
            } finally {
                pw.flush();
                pw.close();
                out.close();
            }
        } catch (final IOException e) {
            ErlLogger.warn(e);
        }
        return reportFile;
    }

}

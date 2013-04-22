/*******************************************************************************
 * Copyright (c) 2004-2010 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.launch;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;

import org.erlide.backend.BackendCore;
import org.erlide.launch.debug.model.ErtsProcess;
import org.erlide.runtime.ErlSystemStatus;
import org.erlide.runtime.IErlRuntime;
import org.erlide.util.ErlLogger;
import org.erlide.util.LogUtil;
import org.erlide.util.MessageReporter;
import org.erlide.util.MessageReporter.ReporterPosition;
import org.erlide.util.SystemConfiguration;

final public class ErtsWatcher implements Runnable {
    private final Process process;
    private final String workingDir;
    private final String nodeName;

    FilenameFilter filter = new FilenameFilter() {
        @Override
        public boolean accept(final File dir, final String name) {
            return name.matches("^core.[0-9]+$");
        }
    };
    private final ErtsProcess ertsProcess;

    public ErtsWatcher(final String nodeName, final String workingDir,
            final Process process, final ErtsProcess ertsProcess) {
        this.nodeName = nodeName;
        this.workingDir = workingDir;
        this.process = process;
        this.ertsProcess = ertsProcess;
    }

    @Override
    @SuppressWarnings("boxing")
    public void run() {
        IErlRuntime runtime = null;
        do {
            runtime = BackendCore.getBackendManager().getByProcess(ertsProcess);
        } while (runtime == null);
        do {
            try {
                final int v = process.waitFor();
                final String msg = String.format(
                        "Backend '%s' terminated with exit code %d.", nodeName,
                        v);

                String report = null;
                if (shouldCreateReport(v)) {
                    ErlLogger.error(msg);

                    final ErlSystemStatus status = runtime.getSystemStatus();
                    ErlLogger.error("Last system status was:\n %s",
                            status != null ? status.prettyPrint() : "null");

                    report = createReport(v, msg);
                    final String reportMsg = report != null ? "\n\n"
                            + "An error log has been created at "
                            + report
                            + ".\nPlease report the problem so that we can fix it.\n"
                            + (SystemConfiguration
                                    .hasFeatureEnabled("erlide.ericsson.user") ? ""
                                    : "http://www.assembla.com/spaces/erlide/support/tickets")
                            : "";
                    final String bigMsg = msg
                            + "\n\n"
                            + "This error is not recoverable, please restart your Eclipse instance."
                            + reportMsg;
                    MessageReporter.showError(bigMsg, ReporterPosition.CENTER);

                } else {
                    ErlLogger.info(msg);
                }
                // FIXME backend.setExitStatus(v);
                return;
            } catch (final InterruptedException e) {
            }
        } while (true);
    }

    private boolean shouldCreateReport(final int v) {
        // 129 = SIGHUP (probably logout, ignore)
        // 143 = SIGTERM (probably logout, ignore)
        // 137 = SIGKILL (probably killed by user)
        return v > 1 && v != 143 && v != 129 && v != 137;
    }

    private String createReport(final int v, final String msg) {
        final String plog = LogUtil.fetchPlatformLog();
        final String elog = LogUtil.fetchErlideLog();
        final String slog = LogUtil.fetchStraceLog(workingDir + "/" + nodeName
                + ".strace");
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
                if (slog.length() > 0) {
                    pw.println(delim);
                    pw.println(elog);
                }
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

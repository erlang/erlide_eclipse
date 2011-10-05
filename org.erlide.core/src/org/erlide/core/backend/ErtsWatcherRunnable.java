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
package org.erlide.core.backend;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;

import org.erlide.core.common.CommonUtils;
import org.erlide.core.common.LogUtil;
import org.erlide.jinterface.ErlLogger;

final public class ErtsWatcherRunnable implements Runnable {
    private final Process process;
    private final String workingDir;
    private final String nodeName;

    FilenameFilter filter = new FilenameFilter() {
        public boolean accept(final File dir, final String name) {
            return name.matches("^core.[0-9]+$");
        }
    };

    public ErtsWatcherRunnable(final String nodeName, final String workingDir,
            final Process process) {
        this.nodeName = nodeName;
        this.workingDir = workingDir;
        this.process = process;
    }

    @SuppressWarnings("boxing")
    public void run() {
        try {
            final int v = process.waitFor();
            final String msg = "Backend '%s' terminated with exit code %d.";
            ErlLogger.error(msg, nodeName, v);

            // 129 = SIGHUP (probably logout, ignore)
            // 143 = SIGTERM (probably logout, ignore)
            // 137 = SIGKILL (probably killed by user)
            if (v > 1 && v != 143 && v != 129 && v != 137
                    && CommonUtils.isEricssonUser()) {
                createReport(v, msg);
            }
            // FIXME backend.setExitStatus(v);
        } catch (final InterruptedException e) {
            ErlLogger.warn("BackendImpl watcher was interrupted");
        }
    }

    private void createReport(final int v, final String msg) {
        final String plog = LogUtil.fetchPlatformLog();
        final String elog = LogUtil.fetchErlideLog();
        final String slog = LogUtil.fetchStraceLog(workingDir + "/" + nodeName
                + ".strace");
        final String delim = "\n==================================\n";
        final File report = new File(LogUtil.getReportFile());
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
    }

}

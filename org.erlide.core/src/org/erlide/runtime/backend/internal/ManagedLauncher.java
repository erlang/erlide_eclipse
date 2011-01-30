package org.erlide.runtime.backend.internal;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.util.StringUtils;
import org.erlide.jinterface.backend.IDisposable;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.ErtsProcess;

public class ManagedLauncher implements IDisposable {

    private Process runtime;
    private final ILaunch launch;
    private IStreamsProxy proxy;

    public ManagedLauncher(final ILaunch aLaunch, final RuntimeInfo info,
            final Map<String, String> env) {
        Assert.isNotNull(aLaunch);
        launch = aLaunch;
        proxy = null;
        startRuntime(info, env);
    }

    public void stop() {
        if (runtime != null) {
            runtime.destroy();
        }
    }

    public void dispose() {
        stop();
    }

    private void startRuntime(final RuntimeInfo info,
            final Map<String, String> my_env) {
        if (info == null) {
            ErlLogger.error("Trying to start backend with null info");
            return;
        }

        String[] cmds = info.getCmdLine();
        cmds = adjustCmdlineForCoredump(cmds);
        final File workingDirectory = new File(info.getWorkingDir());
        ErlLogger.debug("START node :> " + Arrays.toString(cmds) + " *** "
                + workingDirectory);

        runtime = createRuntime(my_env, cmds, workingDirectory);

        final ErtsProcess erts = new ErtsProcess(launch, runtime,
                info.getNodeName(), null);
        launch.addProcess(erts);
        proxy = erts.getStreamsProxy();

        ErlLogger.debug(runtime.toString());
        checkIfRuntimeIsRunning();

        startWatcher(info, workingDirectory);
    }

    private Process createRuntime(final Map<String, String> my_env,
            final String[] cmds, final File workingDirectory) {
        final ProcessBuilder builder = new ProcessBuilder(cmds);
        builder.directory(workingDirectory);
        final Map<String, String> env = builder.environment();
        if (!ErlideUtil.isOnWindows() && ErlideUtil.isEricssonUser()) {
            env.put("TCL_LIBRARY", "/usr/share/tcl/tcl8.4/");
        }
        if (my_env != null) {
            env.putAll(my_env);
        }
        try {
            return builder.start();
        } catch (final IOException e1) {
            e1.printStackTrace();
        }
        return null;
    }

    private void checkIfRuntimeIsRunning() {
        try {
            ErlLogger.debug("exit code: %d", runtime.exitValue());
        } catch (final IllegalThreadStateException e) {
            ErlLogger.debug("process is running");
        }
    }

    private void startWatcher(final RuntimeInfo info,
            final File workingDirectory) {
        final Runnable watcher = new WatcherRunnable(info, workingDirectory);
        final Thread thread = new Thread(null, watcher, "Backend watcher");
        thread.setDaemon(false);
        thread.setPriority(Thread.MIN_PRIORITY);
        thread.start();
    }

    public IStreamsProxy getStreamsProxy() {
        return proxy;
    }

    private String[] adjustCmdlineForCoredump(String[] cmds) {
        final String dump = System.getenv("erlide.internal.coredump");
        if ("true".equals(dump) && !ErlideUtil.isOnWindows()
                && ErlideUtil.isEricssonUser()) {
            final String cmd = StringUtils.joinWithSpaces(cmds);
            cmds = new String[] { "tcsh", "-c",
                    "limit coredumpsize unlimited ;" + " exec " + cmd + " +d" };
        }
        return cmds;
    }

    private final class WatcherRunnable implements Runnable {
        private final RuntimeInfo info;
        private final File workingDirectory;
        FilenameFilter filter = new FilenameFilter() {
            public boolean accept(final File dir, final String name) {
                return name.matches("^core.[0-9]+$");
            }
        };

        private WatcherRunnable(final RuntimeInfo info,
                final File workingDirectory) {
            this.info = info;
            this.workingDirectory = workingDirectory;
        }

        @SuppressWarnings("boxing")
        public void run() {
            try {
                deleteOldCoreDumps();

                final int v = runtime.waitFor();
                final String msg = "Backend '%s' terminated with exit code %d.";
                ErlLogger.error(msg, info.getNodeName(), v);

                // 129 = SIGHUP (probably logout, ignore)
                // 143 = SIGTERM (probably logout, ignore)
                // 137 = SIGKILL (probably killed by user)
                if (v > 1 && v != 143 && v != 129 && v != 137
                        && ErlideUtil.isEricssonUser()) {
                    createReport(info, v, msg);
                }
                // FIXME backend.setExitStatus(v);
            } catch (final InterruptedException e) {
                ErlLogger.warn("Backend watcher was interrupted");
            }
        }

        private void createReport(final RuntimeInfo ainfo, final int v,
                final String msg) {
            String createdDump = null;
            createdDump = createCoreDump(createdDump);

            final String plog = ErlideUtil.fetchPlatformLog();
            final String elog = ErlideUtil.fetchErlideLog();
            final String slog = ErlideUtil.fetchStraceLog(ainfo.getWorkingDir()
                    + "/" + ainfo.getNodeName() + ".strace");
            final String delim = "\n==================================\n";
            final File report = new File(ErlideUtil.getReportFile());
            try {
                report.createNewFile();
                final OutputStream out = new FileOutputStream(report);
                final PrintWriter pw = new PrintWriter(out);
                try {
                    pw.println(String.format(msg, ainfo.getNodeName(), v));
                    pw.println(System.getProperty("user.name"));
                    if (createdDump != null) {
                        pw.println("Core dump file: " + createdDump);
                    }
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

        private String createCoreDump(String createdDump) {
            final File[] dumps = getCoreDumpFiles();
            if (dumps.length != 0) {
                final File dump = dumps[0];
                final File dest = new File(ErlideUtil.getReportLocation() + "/"
                        + dump.getName());
                try {
                    move(dump, dest);
                    createdDump = dest.getPath();
                } catch (final IOException e) {
                    final String errmsg = "Core dump file %s could not be moved to %s";
                    ErlLogger.warn(errmsg, dump.getPath(), dest.getPath());
                }
            }
            return createdDump;
        }

        private void move(final File in, final File out) throws IOException {
            final InputStream ins = new FileInputStream(in);
            final OutputStream outs = new FileOutputStream(out);
            try {
                final byte[] buf = new byte[1024];
                int len;
                while ((len = ins.read(buf)) > 0) {
                    outs.write(buf, 0, len);
                }
            } finally {
                ins.close();
                outs.close();
                in.delete();
            }
        }

        private void deleteOldCoreDumps() {
            final File[] fs = getCoreDumpFiles();
            if (fs == null) {
                return;
            }
            for (final File f : fs) {
                f.delete();
            }
        }

        private File[] getCoreDumpFiles() {
            final File[] fs = workingDirectory.listFiles(filter);
            return fs;
        }
    }
}

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

	Process fRuntime;
	private final ILaunch launch;
	private IStreamsProxy proxy;

	public ManagedLauncher(final ILaunch aLaunch) {
		Assert.isNotNull(aLaunch);
		launch = aLaunch;
		proxy = null;
	}

	public void stop() {
		if (fRuntime != null) {
			fRuntime.destroy();
		}
	}

	public void dispose() {
		stop();
	}

	public void startRuntime(final RuntimeInfo info,
			final Map<String, String> my_env) {
		if (info == null) {
			ErlLogger.error("Trying to start backend with null info");
			return;
		}

		String[] cmds = info.getCmdLine();
		String dump = System.getenv("erlide.internal.coredump");
		if ("true".equals(dump) && !ErlideUtil.isOnWindows()
				&& ErlideUtil.isEricssonUser()) {
			final String cmd = StringUtils.joinWithSpaces(cmds);
			cmds = new String[] { "tcsh", "-c",
					"limit coredumpsize unlimited ;" + " exec " + cmd + " +d" };
		}

		final File workingDirectory = new File(info.getWorkingDir());
		ErlLogger.debug("START node :> " + Arrays.toString(cmds) + " *** "
				+ workingDirectory);

		ProcessBuilder builder = new ProcessBuilder(cmds);
		builder.directory(workingDirectory);
		Map<String, String> env = builder.environment();
		if (!ErlideUtil.isOnWindows() && ErlideUtil.isEricssonUser()) {
			env.put("TCL_LIBRARY", "/usr/share/tcl/tcl8.4/");
		}
		if (my_env != null) {
			env.putAll(my_env);
		}
		try {
			fRuntime = builder.start();
		} catch (IOException e1) {
			e1.printStackTrace();
			return;
		}

		ErtsProcess erts = new ErtsProcess(launch, fRuntime,
				info.getNodeName(), null);
		launch.addProcess(erts);
		proxy = erts.getPrivateStreamsProxy();

		ErlLogger.debug(fRuntime.toString());
		try {
			ErlLogger.debug("exit code: %d", fRuntime.exitValue());
		} catch (IllegalThreadStateException e) {
			ErlLogger.debug("process is running");
		}

		startWatcher(info, workingDirectory);
	}

	private void startWatcher(final RuntimeInfo info,
			final File workingDirectory) {
		final Runnable watcher = new Runnable() {
			@SuppressWarnings("boxing")
			public void run() {
				try {
					deleteOldCoreDumps(workingDirectory);

					final int v = fRuntime.waitFor();
					final String msg = "Backend '%s' terminated with exit code %d.";
					ErlLogger.error(msg, info.getNodeName(), v);

					// 129 = SIGHUP (probably logout, ignore)
					// 143 = SIGTERM (probably logout, ignore)
					// 137 = SIGKILL (probably killed by user)
					if ((v > 1) && (v != 143) && (v != 129) && (v != 137)
							&& ErlideUtil.isEricssonUser()) {
						createReport(info, workingDirectory, v, msg);
					}
					// FIXME backend.setExitStatus(v);
				} catch (final InterruptedException e) {
					ErlLogger.warn("Backend watcher was interrupted");
				}
			}

			private void createReport(final RuntimeInfo ainfo,
					final File aworkingDirectory, final int v, final String msg) {
				String createdDump = null;
				createdDump = createCoreDump(aworkingDirectory, createdDump);

				final String plog = ErlideUtil.fetchPlatformLog();
				final String elog = ErlideUtil.fetchErlideLog();
				final String slog = ErlideUtil.fetchStraceLog(ainfo
						.getWorkingDir()
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

			private String createCoreDump(final File workingDirectory,
					String createdDump) {
				File[] dumps = getCoreDumpFiles(workingDirectory);
				if (dumps.length != 0) {
					File dump = dumps[0];
					final File dest = new File(ErlideUtil.getReportLocation()
							+ "/" + dump.getName());
					try {
						move(dump, dest);
						createdDump = dest.getPath();
					} catch (IOException e) {
						final String errmsg = "Core dump file %s could not be moved to %s";
						ErlLogger.warn(errmsg, dump.getPath(), dest.getPath());
					}
				}
				return createdDump;
			}

			private void move(final File in, final File out) throws IOException {
				InputStream ins = new FileInputStream(in);
				OutputStream outs = new FileOutputStream(out);
				try {
					byte[] buf = new byte[1024];
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

			private void deleteOldCoreDumps(final File workingDirectory) {
				File[] fs = getCoreDumpFiles(workingDirectory);
				if (fs == null) {
					return;
				}
				for (File f : fs) {
					f.delete();
				}
			}

			FilenameFilter filter = new FilenameFilter() {
				public boolean accept(final File dir, final String name) {
					return name.matches("^core.[0-9]+$");
				}
			};

			private File[] getCoreDumpFiles(final File workingDirectory) {
				File[] fs = workingDirectory.listFiles(filter);
				return fs;
			}
		};
		final Thread thread = new Thread(null, watcher, "Backend watcher");
		thread.setDaemon(false);
		thread.setPriority(Thread.MIN_PRIORITY);
		thread.start();
	}

	public IStreamsProxy getStreamsProxy() {
		return proxy;
	}

}

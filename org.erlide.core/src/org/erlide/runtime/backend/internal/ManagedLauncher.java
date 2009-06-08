package org.erlide.runtime.backend.internal;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;

import org.eclipse.debug.core.ILaunch;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.IDisposable;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.RuntimeLauncher;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.ErtsProcess;

public class ManagedLauncher implements RuntimeLauncher, IDisposable {

	Process fRuntime;
	Backend backend;
	private ILaunch launch;

	public ManagedLauncher(ILaunch aLaunch) {
		launch = aLaunch;
	}

	public void setBackend(final Backend backend) {
		this.backend = backend;
	}

	public void stop() {
		if (fRuntime != null) {
			fRuntime.destroy();
		}
	}

	public void dispose() {
		stop();
	}

	public void initializeRuntime() {
		startRuntime(launch);
	}

	private void startRuntime(final ILaunch launch) {
		final RuntimeInfo info = backend.getInfo();
		if (info == null) {
			ErlLogger.error("Trying to start backend '%s' with null info",
					backend.getName());
			return;
		}

		final String cmd = info.getCmdLine();

		ErlLogger.debug("START node :> " + cmd);
		final File workingDirectory = new File(info.getWorkingDir());
		try {
			fRuntime = Runtime.getRuntime().exec(cmd, null, workingDirectory);
			final Runnable watcher = new Runnable() {
				@SuppressWarnings("boxing")
				public void run() {
					try {
						final int v = fRuntime.waitFor();
						final String msg = "Backend '%s' terminated with exit code %d.";
						ErlLogger.error(msg, info.getNodeName(), v);

						if ((v > 1) && ErlideUtil.isEricssonUser()) {
							final String plog = ErlideUtil.fetchPlatformLog();
							final String elog = ErlideUtil.fetchErlideLog();
							final String delim = "\n==================================\n";
							final File report = new File(ErlideUtil
									.getLocation());
							try {
								report.createNewFile();
								final OutputStream out = new FileOutputStream(
										report);
								final PrintWriter pw = new PrintWriter(out);
								try {
									pw.println(String.format(msg, info
											.getNodeName(), v));
									pw.println(System.getProperty("user.name"));
									pw.println(delim);
									pw.println(plog);
									pw.println(delim);
									pw.println(elog);
								} finally {
									pw.flush();
									out.close();
								}
							} catch (final IOException e) {
								ErlLogger.warn(e);
							}

						}

						backend.setExitStatus(v);
					} catch (final InterruptedException e) {
						ErlLogger.warn("Backend watcher was interrupted");
					}
				}
			};
			final Thread thread = new Thread(null, watcher, "Backend watcher");
			thread.setDaemon(false);
			thread.start();

			if (launch != null) {
				ErtsProcess erts = new ErtsProcess(launch, fRuntime, info
						.getNodeName(), null);
				launch.addProcess(erts);
			}
		} catch (final IOException e) {
			ErlLogger.error(e);
		}

		// streamsProxy = new StreamsProxy(runtime, null);
	}

}

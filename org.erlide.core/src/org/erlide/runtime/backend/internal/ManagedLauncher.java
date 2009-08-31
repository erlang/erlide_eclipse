package org.erlide.runtime.backend.internal;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;

import org.eclipse.core.runtime.Assert;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.IDisposable;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.ErtsProcess;

public class ManagedLauncher implements IDisposable {

	Process fRuntime;
	private final ILaunch launch;
	private IStreamsProxy proxy;

	public ManagedLauncher(ILaunch aLaunch) {
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

	public void startRuntime(final RuntimeInfo info) {
		if (info == null) {
			ErlLogger.error("Trying to start backend with null info");
			return;
		}

		String cmd = info.getCmdLine();
		String[] cmds = cmd.split(" ");
		if (System.getProperty("erlide.internal.coredump").equals("true")) {
			cmd = "tcsh -c \"limit coredumpsize unlimited ; exec " + cmd + "\"";
			cmds = new String[]{"tcsh", "-c", "limit coredumpsize unlimited ; exec " + cmd};
		}
		
		final File workingDirectory = new File(info.getWorkingDir());
		ErlLogger.debug("START node :> " + cmd + " *** " + workingDirectory);

		try {
			fRuntime = Runtime.getRuntime().exec(cmds, null, workingDirectory);

			ErtsProcess erts = new ErtsProcess(launch, fRuntime, info
					.getNodeName(), null);
			launch.addProcess(erts);
			proxy = erts.getPrivateStreamsProxy();

			ErlLogger.debug(fRuntime.toString());
			try {
				ErlLogger.debug("exit code: %d", fRuntime.exitValue());
			} catch (IllegalThreadStateException e) {
				ErlLogger.debug("process is running");
			}

			final Runnable watcher = new Runnable() {
				@SuppressWarnings("boxing")
				public void run() {
					try {
						final int v = fRuntime.waitFor();
						final String msg = "Backend '%s' terminated with exit code %d.";
						ErlLogger.error(msg, info.getNodeName(), v);

						// 143=SIGTERM (probably logout, ignore)
						if ((v > 1) && (v != 143)
								&& ErlideUtil.isEricssonUser()) {
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
						// FIXME backend.setExitStatus(v);
					} catch (final InterruptedException e) {
						ErlLogger.warn("Backend watcher was interrupted");
					}
				}
			};
			final Thread thread = new Thread(null, watcher, "Backend watcher");
			thread.setDaemon(false);
			thread.start();

		} catch (final IOException e) {
			ErlLogger.error(e);
		}
	}

	public IStreamsProxy getStreamsProxy() {
		return proxy;
	}

}

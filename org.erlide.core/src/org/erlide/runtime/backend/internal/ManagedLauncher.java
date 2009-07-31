package org.erlide.runtime.backend.internal;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;

import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.debug.internal.core.StreamsProxy;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.IDisposable;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.ErtsProcess;

public class ManagedLauncher implements IDisposable {

	Process fRuntime;
	private final ILaunch launch;
	private ErtsProcess erts;
	private IStreamsProxy proxy;

	public ManagedLauncher(ILaunch aLaunch) {
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

		final String cmd = info.getCmdLine();

		ErlLogger.debug("START node :> " + cmd);
		final File workingDirectory = new File(info.getWorkingDir());
		try {
			fRuntime = Runtime.getRuntime().exec(cmd, null, workingDirectory);
			if (launch == null) {
				proxy = new StreamsProxy(fRuntime, "ISO-8859-1");
			}
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
						// FIXME backend.setExitStatus(v);
					} catch (final InterruptedException e) {
						ErlLogger.warn("Backend watcher was interrupted");
					}
				}
			};
			final Thread thread = new Thread(null, watcher, "Backend watcher");
			thread.setDaemon(false);
			thread.start();

			if (launch != null) {
				erts = new ErtsProcess(launch, fRuntime, info.getNodeName(),
						null);
				launch.addProcess(erts);
			}
		} catch (final IOException e) {
			ErlLogger.error(e);
		}
	}

	public IStreamsProxy getStreamsProxy() {
		return proxy;
	}

	// void startRuntime_2() throws CoreException {
	// ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
	// ILaunchConfigurationType type = manager
	// .getLaunchConfigurationType("org.erlide.core.launch.erlangProcess");
	// ILaunchConfigurationWorkingCopy workingCopy = type.newInstance(null,
	// "some erlang node");
	// workingCopy.setAttribute(ErlLaunchAttributes.NODE_NAME, "mynode");
	// workingCopy.setAttribute(ErlLaunchAttributes.RUNTIME_NAME, "erl5.7.2");
	// ILaunchConfiguration configuration = workingCopy.doSave();
	// configuration
	// .launch(ILaunchManager.RUN_MODE, new NullProgressMonitor());
	// }

}

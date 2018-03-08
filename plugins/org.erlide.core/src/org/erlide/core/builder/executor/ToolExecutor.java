package org.erlide.core.builder.executor;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.externaltools.internal.IExternalToolConstants;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.erlide.core.builder.BuildNotifier;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;

@SuppressWarnings("restriction")
public class ToolExecutor {

	public static class ToolResults {
		public int exit;

		public ToolResults() {
			exit = -1;
		}

		public boolean isFailed() {
			return exit != 0;
		}

		public boolean isCommandNotFound() {
			return exit == -1;
		}

		@Override
		public String toString() {
			return "{{{ exit=" + exit + " }}}";
		}
	}

	@Deprecated
	public ToolResults run_0(final String cmd0, final String args, final String wdir, final ProgressCallback progressCallback, final BuildNotifier notifier) {
		final String cmd = new Path(cmd0).isAbsolute() ? cmd0 : ToolExecutor.getToolLocation(cmd0);

		if (cmd == null) {
			ErlLogger.warn("Tool '" + cmd0 + "' can't be found in $PATH");
			return new ToolResults();
		}

		final ILaunchManager launchManager = DebugPlugin.getDefault().getLaunchManager();
		final ILaunchConfigurationType type = launchManager.getLaunchConfigurationType(IExternalToolConstants.ID_PROGRAM_BUILDER_LAUNCH_CONFIGURATION_TYPE);
		if (type == null) {
			return null;
		}
		try {
			final ILaunchConfigurationWorkingCopy launchConfig = type.newInstance(null, launchManager.generateLaunchConfigurationName("erlTool"));
			launchConfig.setAttribute(IExternalToolConstants.ATTR_LOCATION, cmd);
			launchConfig.setAttribute(IExternalToolConstants.ATTR_TOOL_ARGUMENTS, args);
			launchConfig.setAttribute(IExternalToolConstants.ATTR_WORKING_DIRECTORY, wdir);
			launchConfig.setAttribute(IExternalToolConstants.ATTR_LAUNCH_IN_BACKGROUND, true);
			launchConfig.setAttribute(DebugPlugin.ATTR_CAPTURE_OUTPUT, true);

			final ILaunch myLaunch = launchConfig.launch(ILaunchManager.RUN_MODE, new NullProgressMonitor(), false, false);

			final ToolResults result = new ToolResults();
			if (myLaunch.getProcesses().length == 0) {
				ErlLogger.error("Tool process was not created?!");
				return null;
			}
			final IProcess process = myLaunch.getProcesses()[0];
			process.getStreamsProxy().getOutputStreamMonitor().addListener(new IStreamListener() {

				@Override
				public void streamAppended(final String text, final IStreamMonitor mon) {
					final List<String> lines = Arrays.asList(text.split("\n"));
					if (progressCallback != null) {
						for (final String line : lines) {
							progressCallback.stdout(line);
						}
					}
				}
			});
			process.getStreamsProxy().getErrorStreamMonitor().addListener(new IStreamListener() {

				@Override
				public void streamAppended(final String text, final IStreamMonitor mon) {
					final List<String> lines = Arrays.asList(text.split("\n"));
					if (progressCallback != null) {
						for (final String line : lines) {
							progressCallback.stderr(line);
						}
					}
				}
			});
			boolean done = false;
			try {
				Thread.sleep(60);
			} catch (final InterruptedException e1) {
			}
			final boolean canceled = notifier != null && notifier.isCanceled();
			while (!done && !canceled) {
				try {
					result.exit = process.getExitValue();
					done = true;
				} catch (final Exception e) {
					try {
						Thread.sleep(60);
					} catch (final InterruptedException e1) {
					}
				}
			}
			if (canceled) {
				process.terminate();
			}
			return result;
		} catch (final CoreException e) {
			ErlLogger.error(e);
			return null;
		}
	}

	public static String getToolLocation(final String cmd) {
		// hack because sometimes first call returns an empty value
		String result = null;
		final int MAX_TRIES = 5;
		for (int i = 0; i < MAX_TRIES; i++) {
			result = ToolExecutor.getToolLocation_1(cmd);
			if (result != null) {
				return result;
			}
			try {
				Thread.sleep(60);
			} catch (final InterruptedException e) {
				// ignore
			}
		}
		return result;
	}

	private static String getToolLocation_1(final String cmd) {
		if (SystemConfiguration.getInstance().isOnWindows()) {
			return ToolExecutor.getWindowsToolLocation(cmd);
		}
		return ToolExecutor.getUnixToolLocation(cmd);
	}

	private static String getUnixToolLocation(final String cmd) {
		final ToolProgressCallback callback = new ToolProgressCallback();
		final String[] args = { "-c", "which " + cmd };
		new ToolExecutor().run("/bin/sh", args, null, callback, null);
		return callback.result;
	}

	private static String getWindowsToolLocation(final String cmd) {
		final ToolProgressCallback callback = new ToolProgressCallback();
		final String[] args = { "/c", "where " + cmd };
		new ToolExecutor().run("c:\\Windows\\System32\\cmd.exe", args, null, callback, null);
		if (callback.result == null) {
			return null;
		}
		return callback.result.replace('\\', '/');
	}

	static class ToolProgressCallback implements ProgressCallback {
		String result;

		@Override
		public void stdout(final String line) {
			if (result == null) {
				result = line;
			}
		}

		@Override
		public void stderr(final String line) {
		}
	}

	public ToolResults run(final String cmd0, final String[] args, final String wdir, final ProgressCallback cb, final BuildNotifier bn) {
		final ToolResults result = new ToolResults();

		final List<String> cmds = new ArrayList<>();
		final String cmd = new Path(cmd0).isAbsolute() ? cmd0 : ToolExecutor.getToolLocation(cmd0);

		if (cmd == null) {
			result.exit = -1;
			return result;
		}

		cmds.add(cmd);
		cmds.addAll(Arrays.asList(args));
		final ProcessBuilder builder = new ProcessBuilder(cmds);
		if (wdir != null) {
			builder.directory(new File(wdir));
		}
		try {
			final Process process = builder.start();
			final InputStream is = process.getInputStream();
			final InputStreamReader isr = new InputStreamReader(is);
			try (final BufferedReader br = new BufferedReader(isr)) {
				String line;
				while ((line = br.readLine()) != null) {
					if (cb != null) {
						cb.stdout(line);
					}
				}
			}

			final InputStream es = process.getErrorStream();
			final InputStreamReader esr = new InputStreamReader(es);
			try (final BufferedReader ebr = new BufferedReader(esr)) {
				String line;
				while ((line = ebr.readLine()) != null) {
					if (cb != null) {
						cb.stderr(line);
					}
				}
			}

			result.exit = process.waitFor();
		} catch (final Exception e) {
			ErlLogger.error("Could not execute: %s", cmd);
			ErlLogger.error(e);
		}
		return result;
	}

}

package org.erlide.core.executor;

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
import org.erlide.core.internal.builder.BuildNotifier;
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
            return "{{{ exit=" + exit + "}}}";
        }
    }

    public ToolResults run(final String cmd0, final String args, final String wdir,
            final ProgressCallback progressCallback, final BuildNotifier notifier) {
        final String cmd = new Path(cmd0).isAbsolute() ? cmd0 : getToolLocation(cmd0);

        if (cmd == null) {
            ErlLogger.warn("Tool '" + cmd0 + "' can't be found in $PATH");
            return new ToolResults();
        }

        final ILaunchManager launchManager = DebugPlugin.getDefault().getLaunchManager();
        final ILaunchConfigurationType type = launchManager
                .getLaunchConfigurationType(IExternalToolConstants.ID_PROGRAM_BUILDER_LAUNCH_CONFIGURATION_TYPE);

        try {
            final ILaunchConfigurationWorkingCopy launchConfig = type.newInstance(null,
                    launchManager.generateLaunchConfigurationName("erlTool"));
            launchConfig.setAttribute(IExternalToolConstants.ATTR_LOCATION, cmd);
            launchConfig.setAttribute(IExternalToolConstants.ATTR_TOOL_ARGUMENTS, args);
            launchConfig
                    .setAttribute(IExternalToolConstants.ATTR_WORKING_DIRECTORY, wdir);
            launchConfig.setAttribute(IExternalToolConstants.ATTR_LAUNCH_IN_BACKGROUND,
                    true);
            launchConfig.setAttribute(DebugPlugin.ATTR_CAPTURE_OUTPUT, true);

            final ILaunch myLaunch = launchConfig.launch(ILaunchManager.RUN_MODE,
                    new NullProgressMonitor(), false, false);

            final ToolResults result = new ToolResults();
            if (myLaunch.getProcesses().length == 0) {
                ErlLogger.error("Tool process was not created?!");
                return null;
            }
            final IProcess process = myLaunch.getProcesses()[0];
            process.getStreamsProxy().getOutputStreamMonitor()
                    .addListener(new IStreamListener() {

                        @Override
                        public void streamAppended(final String text,
                                final IStreamMonitor mon) {
                            final List<String> lines = Arrays.asList(text.split("\n"));
                            if (progressCallback != null) {
                                for (final String line : lines) {
                                    progressCallback.stdout(line);
                                }
                            }
                        }
                    });
            process.getStreamsProxy().getErrorStreamMonitor()
                    .addListener(new IStreamListener() {

                        @Override
                        public void streamAppended(final String text,
                                final IStreamMonitor mon) {
                            final List<String> lines = Arrays.asList(text.split("\n"));
                            if (progressCallback != null) {
                                for (final String line : lines) {
                                    progressCallback.stderr(line);
                                }
                            }
                        }
                    });
            boolean done = false;
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

    public ToolResults run(final String cmd0, final String args, final String wdir,
            final BuildNotifier notifier) {
        return run(cmd0, args, wdir, null, notifier);
    }

    public static String getToolLocation(final String cmd) {
        // hack because sometimes first call returns an empty value
        String result;
        final int MAX_TRIES = 5;
        for (int i = 1; i < MAX_TRIES; i++) {
            result = getToolLocation_1(cmd);
            if (result != null) {
                return result;
            }
        }
        result = getToolLocation_1(cmd);
        return result;
    }

    private static String getToolLocation_1(final String cmd) {
        if (SystemConfiguration.getInstance().isOnWindows()) {
            return getWindowsToolLocation(cmd);
        }
        return getUnixToolLocation(cmd);
    }

    private static String getUnixToolLocation(final String cmd) {
        final ToolProgressCallback callback = new ToolProgressCallback();
        new ToolExecutor().run("/bin/sh", "-c \"which " + cmd + "\"", null, callback,
                null);
        return callback.result;
    }

    private static String getWindowsToolLocation(final String cmd) {
        final ToolProgressCallback callback = new ToolProgressCallback();
        new ToolExecutor().run("c:\\Windows\\System32\\cmd.exe", "/c \"where " + cmd
                + "\"", null, callback, null);
        if (callback.result == null) {
            return null;
        }
        return callback.result.replace('\\', '/');
    }

    static class ToolProgressCallback implements ProgressCallback {
        String result = null;

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

}

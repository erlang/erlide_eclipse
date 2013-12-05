package org.erlide.core.executor;

import java.util.Arrays;
import java.util.Collection;
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
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;

import com.google.common.collect.Lists;

@SuppressWarnings("restriction")
public class ToolExecutor {

    public static class ToolResults {
        public Collection<String> output;
        public Collection<String> error;
        public int exit;

        public ToolResults() {
            output = Lists.newArrayList();
            error = Lists.newArrayList();
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
            return "{{{\n  " + output + "\n  " + error + "\n  " + exit + "\n}}}";
        }
    }

    public ToolResults run(final String cmd0, final String args, final String wdir,
            final Procedure1<String> progressCallback) {
        final String cmd = new Path(cmd0).isAbsolute() ? cmd0 : getToolLocation(cmd0);

        if (cmd == null) {
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
            final IProcess process = myLaunch.getProcesses()[0];
            process.getStreamsProxy().getOutputStreamMonitor()
                    .addListener(new IStreamListener() {

                        @Override
                        public void streamAppended(final String text,
                                final IStreamMonitor monitor) {
                            final List<String> lines = Arrays.asList(text.split("\n"));
                            result.output.addAll(lines);
                            if (progressCallback != null) {
                                for (final String line : lines) {
                                    progressCallback.apply(line);
                                }
                            }
                        }
                    });
            process.getStreamsProxy().getErrorStreamMonitor()
                    .addListener(new IStreamListener() {

                        @Override
                        public void streamAppended(final String text,
                                final IStreamMonitor monitor) {
                            result.error.addAll(Arrays.asList(text.split("\n")));
                        }
                    });
            boolean done = false;
            while (!done) {
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
            return result;
        } catch (final CoreException e) {
            ErlLogger.error(e);
            return null;
        }
    }

    public ToolResults run(final String cmd0, final String args, final String wdir) {
        return run(cmd0, args, wdir, null);
    }

    public String getToolLocation(final String cmd) {
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
        if (result == null) {
            ErlLogger.warn("Tool '%s' not found in $PATH!", cmd);
        }
        return result;
    }

    private String getToolLocation_1(final String cmd) {
        if (SystemConfiguration.getInstance().isOnWindows()) {
            return getWindowsToolLocation(cmd);
        }
        return getUnixToolLocation(cmd);
    }

    private String getUnixToolLocation(final String cmd) {
        final ToolResults tool = run("/bin/sh", "-c \"which " + cmd + "\"", null);
        if (tool.output.isEmpty()) {
            return null;
        }
        return tool.output.iterator().next();
    }

    private String getWindowsToolLocation(final String cmd) {
        final ToolResults tool = run("c:\\Windows\\System32\\cmd.exe", "/c \"where "
                + cmd + "\"", null);
        if (tool.output.isEmpty()) {
            return null;
        }
        return tool.output.iterator().next();
    }

}

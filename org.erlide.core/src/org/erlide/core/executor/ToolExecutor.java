package org.erlide.core.executor;

import java.util.Arrays;
import java.util.Collection;

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

        @Override
        public String toString() {
            return "{{{\n  " + output + "\n  " + error + "\n  " + exit
                    + "\n}}}";
        }
    }

    public ToolResults runFromPath(final String cmd, final String args,
            final String wdir) {
        final String cmd1 = new Path(cmd).isAbsolute() ? cmd
                : getToolLocation(cmd);
        return run(cmd1, args, wdir);
    }

    public ToolResults run(final String cmd, final String args,
            final String wdir) {

        final ILaunchManager launchManager = DebugPlugin.getDefault()
                .getLaunchManager();
        final ILaunchConfigurationType type = launchManager
                .getLaunchConfigurationType(IExternalToolConstants.ID_PROGRAM_BUILDER_LAUNCH_CONFIGURATION_TYPE);

        try {
            final ILaunchConfigurationWorkingCopy launchConfig = type
                    .newInstance(null, launchManager
                            .generateLaunchConfigurationName("erlTool"));
            launchConfig
                    .setAttribute(IExternalToolConstants.ATTR_LOCATION, cmd);
            launchConfig.setAttribute(
                    IExternalToolConstants.ATTR_TOOL_ARGUMENTS, args);
            launchConfig.setAttribute(
                    IExternalToolConstants.ATTR_WORKING_DIRECTORY, wdir);
            launchConfig.setAttribute(
                    IExternalToolConstants.ATTR_LAUNCH_IN_BACKGROUND, true);
            launchConfig.setAttribute(DebugPlugin.ATTR_CAPTURE_OUTPUT, true);

            final ILaunch myLaunch = launchConfig.launch(
                    ILaunchManager.RUN_MODE, new NullProgressMonitor(), false,
                    false);

            final ToolResults result = new ToolResults();
            final IProcess process = myLaunch.getProcesses()[0];
            process.getStreamsProxy().getOutputStreamMonitor()
                    .addListener(new IStreamListener() {

                        @Override
                        public void streamAppended(final String text,
                                final IStreamMonitor monitor) {
                            result.output.addAll(Arrays.asList(text.split("\n")));
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
            while (result.exit < 0) {
                try {
                    result.exit = process.getExitValue();
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

    public String getToolLocation(final String cmd) {
        // hack because sometimes first call returns an empty value
        final String result = getToolLocation_1(cmd);
        if (result != null) {
            return result;
        }
        return getToolLocation_1(cmd);
    }

    public String getToolLocation_1(final String cmd) {
        if (SystemConfiguration.getInstance().isOnWindows()) {
            return getWindowsToolLocation(cmd);
        }
        return getUnixToolLocation(cmd);
    }

    public String getUnixToolLocation(final String cmd) {
        final ToolResults tool = run("/bin/bash", "-c \"which " + cmd + "\"",
                null);
        if (tool.output.isEmpty()) {
            ErlLogger.warn("Tool %s not on $PATH!", cmd);
            return null;
        }
        return tool.output.iterator().next();
    }

    public String getWindowsToolLocation(final String cmd) {
        final ToolResults tool = run("c:\\Windows\\System32\\cmd.exe",
                "/c \"where " + cmd + "\"", null);
        if (tool.output.isEmpty()) {
            ErlLogger.warn("Tool %s not on $PATH!", cmd);
            return null;
        }
        return tool.output.iterator().next();
    }

}

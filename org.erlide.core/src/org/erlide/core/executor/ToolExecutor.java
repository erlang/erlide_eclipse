package org.erlide.core.executor;

import java.util.Arrays;
import java.util.Collection;

import org.eclipse.core.externaltools.internal.IExternalToolConstants;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
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
    }

    public ToolResults run(final String cmd, final String args,
            final String wdir) {

        final ILaunchManager launchManager = DebugPlugin.getDefault()
                .getLaunchManager();
        final ILaunchConfigurationType type = launchManager
                .getLaunchConfigurationType("org.eclipse.ui.externaltools.ProgramLaunchConfigurationType");

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
            launchConfig.setAttribute(IExternalToolConstants.ATTR_SHOW_CONSOLE,
                    true);
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
            while (!process.isTerminated()) {
                try {
                    Thread.sleep(10);
                } catch (final InterruptedException e) {
                }
            }
            result.exit = process.getExitValue();
            return result;
        } catch (final CoreException e) {
            ErlLogger.error(e);
            return null;
        }

    }

    public String getToolLocation(final String cmd) {
        if (SystemConfiguration.getInstance().isOnWindows()) {
            return getWindowsToolLocation(cmd);
        }
        return getUnixToolLocation(cmd);
    }

    public String getUnixToolLocation(final String cmd) {
        final ToolResults make = run("/bin/bash", "-c \"which " + cmd + "\"",
                null);
        if (make.output.isEmpty()) {
            return null;
        }
        return make.output.iterator().next();
    }

    public String getWindowsToolLocation(final String cmd) {
        final ToolResults make = run("c:\\Windows\\System32\\cmd.exe",
                "-c \"where " + cmd + "\"", null);
        if (make.output.isEmpty()) {
            return null;
        }
        return make.output.iterator().next();
    }

}

/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Vlad Dumitrescu
 * Jakob Cederlund
 *******************************************************************************/
package org.erlide.launch;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;
import org.eclipse.debug.core.model.IProcess;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendData;
import org.erlide.backend.IBackend;
import org.erlide.model.BeamLocator;
import org.erlide.runtime.ErlRuntimeAttributes;
import org.erlide.runtime.epmd.IEpmdWatcher;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.util.HostnameUtils;
import org.erlide.utils.ErlLogger;
import org.erlide.utils.SystemConfiguration;

import com.google.common.collect.Maps;

public class ErlangLaunchDelegate implements ILaunchConfigurationDelegate {

    @Override
    public void launch(final ILaunchConfiguration config, final String mode,
            final ILaunch launch, final IProgressMonitor monitor)
            throws CoreException {
        final boolean doContinue = preLaunch(config, mode, launch, monitor);
        if (!doContinue) {
            return;
        }
        final IBackend backend = doLaunch(config, mode, launch, monitor);
        if (backend == null) {
            return;
        }
        postLaunch(mode, backend, monitor);
    }

    protected boolean preLaunch(final ILaunchConfiguration config,
            final String mode, final ILaunch launch,
            final IProgressMonitor monitor) throws CoreException {
        return true;
    }

    protected IBackend doLaunch(final ILaunchConfiguration config,
            final String mode, final ILaunch launch,
            final IProgressMonitor monitor) throws CoreException {
        RuntimeInfo runtimeInfo = BackendCore.getRuntimeInfoCatalog()
                .getRuntime(
                        config.getAttribute(ErlRuntimeAttributes.RUNTIME_NAME,
                                ""));
        if (runtimeInfo == null) {
            runtimeInfo = BackendCore.getRuntimeInfoCatalog()
                    .getDefaultRuntime();
        }
        if (runtimeInfo == null) {
            // TODO what to do here?
            ErlLogger.error("Can't create backend without a runtime defined!");
            return null;
        }
        final String nodeName = config.getAttribute(
                ErlRuntimeAttributes.NODE_NAME, "");
        BackendData data = new BackendData(runtimeInfo, config, mode,
                shouldManageNode(nodeName, BackendCore.getEpmdWatcher()));
        final RuntimeInfo info = data.getRuntimeInfo();
        if (info == null) {
            ErlLogger.error("Could not find runtime '%s'", data
                    .getRuntimeInfo().getName());
            return null;
        }
        ErlLogger.debug("doLaunch runtime %s", data.getRuntimeInfo().getName());
        ErlLogger.debug("doLaunch cookie %s (%s)", data.getCookie(),
                data.getCookie());

        data = configureBackend(data, config, mode, launch);

        // if (ErlideUtil.isDeveloper()) {
        data.debugPrint();
        // }

        if (data.isManaged()) {
            setCaptureOutput(launch);
            startErtsProcess(launch, data);
        } else {
            ErlLogger.info("Node %s exists already.", data.getNodeName());
        }

        if (!isErlangInternalLaunch(launch)) {
            return BackendCore.getBackendManager().createExecutionBackend(data);
        }
        // The backend was already created
        return null;
    }

    protected void postLaunch(final String mode, final IBackend b,
            final IProgressMonitor monitor) throws CoreException {
    }

    /*
     * Child classes override this to set specific information
     */
    protected BackendData configureBackend(final BackendData data,
            final ILaunchConfiguration config, final String mode,
            final ILaunch launch) {
        data.setLaunch(launch);
        data.setBeamLocator(new BeamLocator());
        return data;
    }

    private void startErtsProcess(final ILaunch launch, final BackendData data) {
        final Process process = startRuntimeProcess(data);
        if (process == null) {
            ErlLogger.debug("Error starting process");
            data.setManaged(false);
            return;
        }
        final Map<String, String> map = Maps.newHashMap();
        map.put("NodeName", data.getNodeName());
        map.put("workingDir", data.getWorkingDir());
        // final ErtsProcess erts = new ErtsProcess(process, data);
        final IProcess erts = DebugPlugin.newProcess(launch, process,
                data.getNodeName(), map);

        ErlLogger.debug("Started erts: %s >> %s", erts.getLabel(),
                data.getNodeName());
    }

    /**
     * used by selfhost plugin
     */
    public void launchInternal(final ILaunchConfiguration configuration,
            final String mode, final ILaunch launch,
            final IProgressMonitor monitor) throws CoreException {
        final ILaunchConfigurationWorkingCopy wc = configuration
                .getWorkingCopy();
        wc.setAttribute(ErlRuntimeAttributes.COOKIE, "erlide");
        launch(wc, mode, launch, monitor);
    }

    private Process startRuntimeProcess(final BackendData data) {
        final String[] cmds = data.getCmdLine();
        final File workingDirectory = new File(data.getWorkingDir());

        try {
            ErlLogger.debug("START node :> " + Arrays.toString(cmds) + " *** "
                    + workingDirectory.getCanonicalPath());
        } catch (final IOException e1) {
            ErlLogger.debug("START node :> " + Arrays.toString(cmds) + " *** "
                    + workingDirectory);
        }

        final ProcessBuilder builder = new ProcessBuilder(cmds);
        builder.directory(workingDirectory);
        setEnvironment(data, builder);
        try {
            Process process = builder.start();
            try {
                final int code = process.exitValue();
                ErlLogger.error(
                        "Could not create runtime (exit code = %d): %s", code,
                        Arrays.toString(cmds));
                process = null;
            } catch (final IllegalThreadStateException e) {
                ErlLogger.debug("process is running");
            }
            return process;
        } catch (final IOException e) {
            ErlLogger.error("Could not create runtime: %s",
                    Arrays.toString(cmds));
            ErlLogger.error(e);
            return null;
        }
    }

    private void setEnvironment(final BackendData data,
            final ProcessBuilder builder) {
        final Map<String, String> env = builder.environment();
        if (!SystemConfiguration.getInstance().isOnWindows()
                && SystemConfiguration.getInstance().hasSpecialTclLib()) {
            env.put("TCL_LIBRARY", "/usr/share/tcl/tcl8.4/");
        }
        if (data.getEnv() != null) {
            env.putAll(data.getEnv());
        }
    }

    private void setCaptureOutput(final ILaunch launch) {
        // important, so that we don't get the "normal" console for the erlide
        // backend
        final String captureOutput = System.getProperty(
                "erlide.console.stdout", "false");
        launch.setAttribute(DebugPlugin.ATTR_CAPTURE_OUTPUT, captureOutput);
    }

    public static boolean isErlangLaunch(final ILaunch aLaunch) {
        try {
            final ILaunchConfiguration cfg = aLaunch.getLaunchConfiguration();
            final ILaunchConfigurationType type = cfg.getType();
            final String id = type.getIdentifier();
            return IErlangLaunchDelegateConstants.CONFIGURATION_TYPE.equals(id)
                    || IErlangLaunchDelegateConstants.CONFIGURATION_TYPE_INTERNAL
                            .equals(id);
        } catch (final CoreException e) {
            ErlLogger.warn(e);
            return false;
        }
    }

    public static boolean isErlangInternalLaunch(final ILaunch aLaunch) {
        try {
            final ILaunchConfiguration cfg = aLaunch.getLaunchConfiguration();
            final ILaunchConfigurationType type = cfg.getType();
            final String id = type.getIdentifier();
            return IErlangLaunchDelegateConstants.CONFIGURATION_TYPE_INTERNAL
                    .equals(id);
        } catch (final CoreException e) {
            ErlLogger.warn(e);
            return false;
        }
    }

    public static boolean shouldManageNode(final String name,
            final IEpmdWatcher epmdWatcher) {
        final int atSignIndex = name.indexOf('@');
        String shortName = name;
        if (atSignIndex > 0) {
            shortName = name.substring(0, atSignIndex);
        }

        boolean isLocal = atSignIndex < 0;
        if (atSignIndex > 0) {
            final String hostname = name.substring(atSignIndex + 1);
            if (HostnameUtils.isThisHost(hostname)) {
                isLocal = true;
            }
        }

        final boolean isRunning = epmdWatcher.hasLocalNode(shortName);
        final boolean result = isLocal && !isRunning;
        return result;
    }

}

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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IBreakpointManager;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;
import org.eclipse.debug.core.model.IProcess;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendData;
import org.erlide.backend.IBackend;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.model.erlang.ModuleKind;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.debug.ErlDebugConstants;
import org.erlide.utils.SystemUtils;

import com.google.common.collect.Maps;

public class ErlangLaunchDelegate implements ILaunchConfigurationDelegate {

    public static final String CONFIGURATION_TYPE_INTERNAL = "org.erlide.core.launch.internal";
    public static final String CONFIGURATION_TYPE = "org.erlide.core.launch.erlangProcess";

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
        BackendData data = new BackendData(BackendCore.getRuntimeInfoManager(),
                config, mode);
        final RuntimeInfo info = data.getRuntimeInfo();
        if (info == null) {
            ErlLogger.error("Could not find runtime '%s'",
                    data.getRuntimeName());
            return null;
        }
        ErlLogger.debug("doLaunch runtime %s (%s)", data.getRuntimeName(), data
                .getRuntimeInfo().getName());
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
        wc.setAttribute(ErlLaunchAttributes.COOKIE, "erlide");
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
        if (!SystemUtils.getInstance().isOnWindows()
                && SystemUtils.getInstance().hasSpecialTclLib()) {
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
            return ErlangLaunchDelegate.CONFIGURATION_TYPE.equals(id)
                    || ErlangLaunchDelegate.CONFIGURATION_TYPE_INTERNAL
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
            return ErlangLaunchDelegate.CONFIGURATION_TYPE_INTERNAL.equals(id);
        } catch (final CoreException e) {
            ErlLogger.warn(e);
            return false;
        }
    }

    public static List<String> addBreakpointProjectsAndModules(
            final Collection<IProject> projects,
            final List<String> interpretedModules) {
        final IBreakpointManager bpm = DebugPlugin.getDefault()
                .getBreakpointManager();
        final List<String> result = new ArrayList<String>(interpretedModules);
        for (final IBreakpoint bp : bpm
                .getBreakpoints(ErlDebugConstants.ID_ERLANG_DEBUG_MODEL)) {
            final IMarker m = bp.getMarker();
            final IResource r = m.getResource();
            final String name = r.getName();
            if (ModuleKind.hasErlExtension(name)) {
                final IProject p = r.getProject();
                if (projects.contains(p)) {
                    final String s = p.getName() + ":" + name;
                    if (!result.contains(s)) {
                        result.add(s);
                    }
                }
            }
        }
        return result;
    }

}

/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IBreakpointManager;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IBreakpoint;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.backend.runtimeinfo.RuntimeInfoManager;
import org.erlide.core.model.erlang.ModuleKind;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.BeamLocator;
import org.erlide.launch.ErlLaunchAttributes;
import org.erlide.launch.ErlangLaunchDelegate;
import org.erlide.launch.debug.ErlDebugConstants;
import org.erlide.utils.SystemUtils;

import com.google.common.collect.Lists;

public final class BackendData extends GenericBackendData {

    public static final String PROJECT_NAME_SEPARATOR = ";";

    private RuntimeInfoManager runtimeInfoManager;
    private BeamLocator beamLocator;

    public BackendData(final RuntimeInfoManager runtimeInfoManager,
            final ILaunchConfiguration config, final String mode) {
        super(config, mode);
        this.runtimeInfoManager = runtimeInfoManager;
        final RuntimeInfo runtimeInfo = runtimeInfoManager
                .getRuntime(getRuntimeName());
        if (runtimeInfo == null) {
            return;
        }
        if (getStringAttribute(ErlLaunchAttributes.EXTRA_ARGS, "").equals("")) {
            setAttribute(ErlLaunchAttributes.EXTRA_ARGS, runtimeInfo.getArgs());
        }
    }

    public BackendData(final RuntimeInfoManager runtimeInfoManager,
            final RuntimeInfo info) {
        super(null, ILaunchManager.RUN_MODE);
        if (info == null) {
            throw new IllegalArgumentException(
                    "BackendData can't be created with null RuntimeInfo");
        }
        this.runtimeInfoManager = runtimeInfoManager;
        setRuntimeName(info.getName());
        setNodeName(info.getNodeName());
        setCookie(info.getCookie());
        setLongName(info.getLongName());

        setAutostart(true);
        setWorkingDir(info.getWorkingDir());
        setExtraArgs(info.getArgs());

        setConsole(true);
        setLoadAllNodes(false);
    }

    private List<IProject> gatherProjects(final String[] projectNames) {
        final List<IProject> projects = Lists.newArrayList();
        for (final String s : projectNames) {
            final IProject project = ResourcesPlugin.getWorkspace().getRoot()
                    .getProject(s);
            if (project == null) {
                ErlLogger.error("Launch: project not found: '%s'!", s);
                continue;
            }
            projects.add(project);
        }
        return projects;
    }

    public static Collection<String> addBreakpointProjectsAndModules(
            final Collection<IProject> projects,
            final Collection<String> interpretedModules2) {
        final IBreakpointManager bpm = DebugPlugin.getDefault()
                .getBreakpointManager();
        final List<String> result = Lists.newArrayList(interpretedModules2);
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

    public RuntimeInfo getRuntimeInfo() {
        RuntimeInfo runtimeInfo = runtimeInfoManager
                .getRuntime(getRuntimeName());
        if (runtimeInfo == null) {
            return null;
        }
        runtimeInfo = RuntimeInfo.copy(runtimeInfo, false);
        runtimeInfo.setNodeName(getNodeName());
        runtimeInfo.setCookie(getCookie());

        runtimeInfo.setStartShell(true);
        final File d = new File(getWorkingDir());
        if (d.isAbsolute()) {
            runtimeInfo.setWorkingDir(getWorkingDir());
        } else {
            final String wspace = ResourcesPlugin.getWorkspace().getRoot()
                    .getLocation().toPortableString();
            runtimeInfo.setWorkingDir(wspace + "/" + getWorkingDir());
        }
        runtimeInfo.setArgs(getExtraArgs());
        runtimeInfo.useLongName(isLongName());
        runtimeInfo.setLoadAllNodes(isLoadAllNodes());
        return runtimeInfo;
    }

    public ILaunch getLaunch() {
        return launch;
    }

    public ILaunchConfiguration asLaunchConfiguration() {
        final ILaunchManager manager = DebugPlugin.getDefault()
                .getLaunchManager();
        final ILaunchConfigurationType type = manager
                .getLaunchConfigurationType(ErlangLaunchDelegate.CONFIGURATION_TYPE_INTERNAL);
        ILaunchConfigurationWorkingCopy workingCopy;
        try {
            final RuntimeInfo info = getRuntimeInfo();
            final String name = info.getNodeName();
            workingCopy = type.newInstance(null, name);
            workingCopy.setAttribute(DebugPlugin.ATTR_CONSOLE_ENCODING,
                    "ISO-8859-1");
            workingCopy.setAttribute(DebugPlugin.ATTR_PROCESS_FACTORY_ID,
                    "org.erlide.core.ertsProcessFactory");

            workingCopy.setAttribute(ErlLaunchAttributes.NODE_NAME,
                    info.getNodeName());
            workingCopy.setAttribute(ErlLaunchAttributes.RUNTIME_NAME,
                    info.getName());
            workingCopy.setAttribute(ErlLaunchAttributes.COOKIE,
                    info.getCookie());
            // workingCopy.setAttribute(ErlLaunchAttributes.CONSOLE,
            // !options.contains(BackendOptions.NO_CONSOLE));
            if (SystemUtils.getInstance().hasFeatureEnabled(
                    "erlide.internal.shortname")) {
                workingCopy.setAttribute(ErlLaunchAttributes.USE_LONG_NAME,
                        false);
                info.useLongName(false);
            }
            return workingCopy;
        } catch (final CoreException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getCookie() {
        return getStringAttribute(ErlLaunchAttributes.COOKIE, "").trim();
    }

    public void setCookie(final String cookie) {
        config.setAttribute(ErlLaunchAttributes.COOKIE, cookie);
    }

    public boolean isMonitored() {
        return getBooleanAttribute(ErlLaunchAttributes.MONITORED, false);
    }

    public void setMonitored(final boolean monitored) {
        config.setAttribute(ErlLaunchAttributes.MONITORED, monitored);
    }

    public boolean isManaged() {
        return getBooleanAttribute(ErlLaunchAttributes.MANAGED, true);
    }

    public void setManaged(final boolean managed) {
        config.setAttribute(ErlLaunchAttributes.MANAGED, managed);
    }

    public boolean isAutostart() {
        return getBooleanAttribute(ErlLaunchAttributes.AUTOSTART, true);
    }

    public void setAutostart(final boolean autostart) {
        config.setAttribute(ErlLaunchAttributes.AUTOSTART, autostart);
    }

    public boolean hasConsole() {
        return getBooleanAttribute(ErlLaunchAttributes.CONSOLE, true);
    }

    public void setConsole(final boolean console) {
        config.setAttribute(ErlLaunchAttributes.CONSOLE, console);
    }

    public boolean isDebug() {
        return debug;
    }

    public void setDebug(final boolean debug) {
        this.debug = debug;
    }

    public Collection<String> getInterpretedModules() {
        final List<String> interpretedModules = getListAttribute(
                ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
                new ArrayList<String>());
        return addBreakpointProjectsAndModules(getProjects(),
                interpretedModules);
    }

    public void setInterpretedModules(
            final Collection<String> interpretedModules) {
        config.setAttribute(ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
                new ArrayList<String>());
    }

    public String getRuntimeName() {
        return getStringAttribute(ErlLaunchAttributes.RUNTIME_NAME,
                runtimeInfoManager.getDefaultRuntimeName());
    }

    public void setRuntimeName(final String name) {
        config.setAttribute(ErlLaunchAttributes.RUNTIME_NAME, name);
    }

    public String getNodeName() {
        return getStringAttribute(ErlLaunchAttributes.NODE_NAME, "");
    }

    public void setNodeName(final String name) {
        config.setAttribute(ErlLaunchAttributes.NODE_NAME, name);
    }

    public boolean isLongName() {
        return getBooleanAttribute(ErlLaunchAttributes.USE_LONG_NAME, true);
    }

    public void setLongName(final boolean longname) {
        config.setAttribute(ErlLaunchAttributes.USE_LONG_NAME, longname);
    }

    public String getExtraArgs() {
        return getStringAttribute(ErlLaunchAttributes.EXTRA_ARGS, "");
    }

    public void setExtraArgs(final String xtra) {
        config.setAttribute(ErlLaunchAttributes.EXTRA_ARGS, xtra);
    }

    public String getWorkingDir() {
        return getStringAttribute(ErlLaunchAttributes.WORKING_DIR,
                ErlLaunchAttributes.DEFAULT_WORKING_DIR);
    }

    public void setWorkingDir(final String dir) {
        config.setAttribute(ErlLaunchAttributes.WORKING_DIR, dir);
    }

    public Map<String, String> getEnv() {
        return getMapAttribute(ILaunchManager.ATTR_ENVIRONMENT_VARIABLES,
                new HashMap<String, String>());
    }

    public InitialCall getInitialCall() {
        final String module = getStringAttribute(ErlLaunchAttributes.MODULE, "");
        final String function = getStringAttribute(
                ErlLaunchAttributes.FUNCTION, "");
        final String args = getStringAttribute(ErlLaunchAttributes.ARGUMENTS,
                "");
        return new InitialCall(module, function, args);
    }

    public Collection<IProject> getProjects() {
        String prjs;
        prjs = getStringAttribute(ErlLaunchAttributes.PROJECTS, "");
        final String[] projectNames = prjs.length() == 0 ? new String[] {}
                : prjs.split(";");
        return gatherProjects(projectNames);
    }

    public int getDebugFlags() {
        return getIntAttribute(ErlLaunchAttributes.DEBUG_FLAGS,
                ErlDebugConstants.DEFAULT_DEBUG_FLAGS);
    }

    public boolean isLoadAllNodes() {
        return getBooleanAttribute(ErlLaunchAttributes.LOAD_ALL_NODES, false);
    }

    public void setLoadAllNodes(final boolean load) {
        config.setAttribute(ErlLaunchAttributes.LOAD_ALL_NODES, load);
    }

    public void setAttribute(final String key, final List<String> value) {
        config.setAttribute(key, value);
    }

    public void setBeamLocator(final BeamLocator beamLocator) {
        this.beamLocator = beamLocator;
    }

    public BeamLocator getBeamLocator() {
        return beamLocator;
    }
}

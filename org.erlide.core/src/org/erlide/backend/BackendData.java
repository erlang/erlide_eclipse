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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
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
import org.erlide.core.model.IBeamLocator;
import org.erlide.core.model.erlang.ModuleKind;
import org.erlide.launch.ErlLaunchAttributes;
import org.erlide.launch.ErlangLaunchDelegate;
import org.erlide.launch.debug.ErlDebugConstants;
import org.erlide.runtime.HostnameUtils;
import org.erlide.runtime.InitialCall;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeInfoManager;
import org.erlide.utils.ErlLogger;
import org.erlide.utils.SystemConfiguration;

import com.google.common.base.Charsets;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;

public final class BackendData extends GenericBackendData implements
        IBackendData {

    private RuntimeInfoManager runtimeInfoManager;
    private IBeamLocator beamLocator;
    private boolean fTransient = false;

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
        setManaged(shouldManageNode(getNodeName()));
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
        setCookie("erlide");
        setLongName(true);

        setAutostart(true);
        setWorkingDir(getDefaultWorkingDir());
        setExtraArgs(info.getArgs());

        setConsole(true);
        setLoadAllNodes(false);
    }

    private String getDefaultWorkingDir() {
        final IWorkspaceRoot wroot = ResourcesPlugin.getWorkspace().getRoot();
        return wroot.getLocation().toPortableString();
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

    @Override
    public RuntimeInfo getRuntimeInfo() {
        RuntimeInfo runtimeInfo = runtimeInfoManager
                .getRuntime(getRuntimeName());
        if (runtimeInfo == null) {
            return null;
        }
        runtimeInfo = RuntimeInfo.copy(runtimeInfo, false);
        runtimeInfo.setArgs(getExtraArgs());
        return runtimeInfo;
    }

    @Override
    public ILaunch getLaunch() {
        return launch;
    }

    @Override
    public ILaunchConfiguration asLaunchConfiguration() {
        final ILaunchManager manager = DebugPlugin.getDefault()
                .getLaunchManager();
        final ILaunchConfigurationType type = manager
                .getLaunchConfigurationType(ErlangLaunchDelegate.CONFIGURATION_TYPE_INTERNAL);
        ILaunchConfigurationWorkingCopy workingCopy;
        try {
            final RuntimeInfo info = getRuntimeInfo();
            final String name = getNodeName();
            workingCopy = type.newInstance(null, name);
            workingCopy.setAttribute(DebugPlugin.ATTR_CONSOLE_ENCODING,
                    Charsets.ISO_8859_1.name());
            workingCopy.setAttribute(DebugPlugin.ATTR_PROCESS_FACTORY_ID,
                    "org.erlide.core.ertsProcessFactory");

            workingCopy.setAttribute(ErlLaunchAttributes.NODE_NAME,
                    getNodeName());
            workingCopy.setAttribute(ErlLaunchAttributes.RUNTIME_NAME,
                    info.getName());
            workingCopy.setAttribute(ErlLaunchAttributes.COOKIE, getCookie());
            // workingCopy.setAttribute(ErlLaunchAttributes.CONSOLE,
            // !options.contains(BackendOptions.NO_CONSOLE));
            workingCopy.setAttribute(ErlLaunchAttributes.USE_LONG_NAME,
                    isLongName());
            workingCopy
                    .setAttribute(ErlLaunchAttributes.INTERNAL, isInternal());

            return workingCopy;
        } catch (final CoreException e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public String getCookie() {
        return getStringAttribute(ErlLaunchAttributes.COOKIE, "").trim();
    }

    @Override
    public void setCookie(final String cookie) {
        config.setAttribute(ErlLaunchAttributes.COOKIE, cookie);
    }

    @Override
    public boolean isManaged() {
        return getBooleanAttribute(ErlLaunchAttributes.MANAGED, true);
    }

    @Override
    public void setManaged(final boolean managed) {
        config.setAttribute(ErlLaunchAttributes.MANAGED, managed);
    }

    @Override
    public boolean isAutostart() {
        return getBooleanAttribute(ErlLaunchAttributes.AUTOSTART, true);
    }

    @Override
    public void setAutostart(final boolean autostart) {
        config.setAttribute(ErlLaunchAttributes.AUTOSTART, autostart);
    }

    @Override
    public boolean useStartShell() {
        return getBooleanAttribute(ErlLaunchAttributes.SHELL, true);
    }

    @Override
    public void setUseStartShell(final boolean shell) {
        config.setAttribute(ErlLaunchAttributes.SHELL, shell);
    }

    @Override
    public boolean hasConsole() {
        return getBooleanAttribute(ErlLaunchAttributes.CONSOLE, true);
    }

    @Override
    public void setConsole(final boolean console) {
        config.setAttribute(ErlLaunchAttributes.CONSOLE, console);
    }

    @Override
    public boolean isDebug() {
        return debug;
    }

    @Override
    public void setDebug(final boolean debug) {
        this.debug = debug;
    }

    @Override
    public Collection<String> getInterpretedModules() {
        final List<String> interpretedModules = getListAttribute(
                ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
                new ArrayList<String>());
        return addBreakpointProjectsAndModules(getProjects(),
                interpretedModules);
    }

    @Override
    public void setInterpretedModules(
            final Collection<String> interpretedModules) {
        config.setAttribute(ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
                new ArrayList<String>());
    }

    @Override
    public String getRuntimeName() {
        return getStringAttribute(ErlLaunchAttributes.RUNTIME_NAME,
                runtimeInfoManager.getDefaultRuntimeName());
    }

    @Override
    public void setRuntimeName(final String name) {
        config.setAttribute(ErlLaunchAttributes.RUNTIME_NAME, name);
    }

    @Override
    public String getNodeName() {
        return getStringAttribute(ErlLaunchAttributes.NODE_NAME, "");
    }

    @Override
    public void setNodeName(String nodeName) {
        if (!validateNodeName(nodeName)) {
            // TODO this still can create a name that isn't valid
            nodeName = nodeName.replaceAll("[^a-zA-Z0-9_-]", "");
        }
        config.setAttribute(ErlLaunchAttributes.NODE_NAME, nodeName);
    }

    public static boolean validateNodeName(final String name) {
        return name != null
                && name.matches("[a-zA-Z0-9_-]+(@[a-zA-Z0-9_.-]+)?");
    }

    @Override
    public boolean isLongName() {
        return getBooleanAttribute(ErlLaunchAttributes.USE_LONG_NAME, true);
    }

    @Override
    public void setLongName(final boolean longname) {
        config.setAttribute(ErlLaunchAttributes.USE_LONG_NAME, longname);
    }

    @Override
    public String getExtraArgs() {
        return getStringAttribute(ErlLaunchAttributes.EXTRA_ARGS, "");
    }

    @Override
    public void setExtraArgs(final String xtra) {
        config.setAttribute(ErlLaunchAttributes.EXTRA_ARGS, xtra);
    }

    @Override
    public String getWorkingDir() {
        return getStringAttribute(ErlLaunchAttributes.WORKING_DIR,
                ErlLaunchAttributes.DEFAULT_WORKING_DIR);
    }

    @Override
    public void setWorkingDir(final String dir) {
        config.setAttribute(ErlLaunchAttributes.WORKING_DIR, dir);
    }

    @Override
    public Map<String, String> getEnv() {
        return getMapAttribute(ILaunchManager.ATTR_ENVIRONMENT_VARIABLES,
                new HashMap<String, String>());
    }

    @Override
    public InitialCall getInitialCall() {
        final String module = getStringAttribute(ErlLaunchAttributes.MODULE, "");
        final String function = getStringAttribute(
                ErlLaunchAttributes.FUNCTION, "");
        final String args = getStringAttribute(ErlLaunchAttributes.ARGUMENTS,
                "");
        return new InitialCall(module, function, args);
    }

    @Override
    public Collection<IProject> getProjects() {
        String prjs;
        prjs = getStringAttribute(ErlLaunchAttributes.PROJECTS, "");
        final String[] projectNames = prjs.length() == 0 ? new String[] {}
                : prjs.split(";");
        return gatherProjects(projectNames);
    }

    @Override
    public int getDebugFlags() {
        return getIntAttribute(ErlLaunchAttributes.DEBUG_FLAGS,
                ErlDebugConstants.DEFAULT_DEBUG_FLAGS);
    }

    @Override
    public boolean isLoadAllNodes() {
        return getBooleanAttribute(ErlLaunchAttributes.LOAD_ALL_NODES, false);
    }

    @Override
    public void setLoadAllNodes(final boolean load) {
        config.setAttribute(ErlLaunchAttributes.LOAD_ALL_NODES, load);
    }

    @Override
    public void setAttribute(final String key, final List<String> value) {
        config.setAttribute(key, value);
    }

    @Override
    public void setBeamLocator(final IBeamLocator beamLocator) {
        this.beamLocator = beamLocator;
    }

    @Override
    public IBeamLocator getBeamLocator() {
        return beamLocator;
    }

    @Override
    public boolean isTransient() {
        return fTransient;
    }

    @Override
    public void setTransient(final boolean value) {
        fTransient = value;
    }

    @Override
    public boolean isInternal() {
        return getBooleanAttribute(ErlLaunchAttributes.INTERNAL, false);
    }

    @Override
    public void setInternal(final boolean value) {
        config.setAttribute(ErlLaunchAttributes.INTERNAL, value);
    }

    @Override
    public String[] getCmdLine() {
        final RuntimeInfo r = getRuntimeInfo();
        final List<String> result = new ArrayList<String>();

        if (hasDetachedConsole() && !isInternal()) {
            if (SystemConfiguration.getInstance().isOnWindows()) {
                result.add("cmd.exe");
                result.add("/c");
                result.add("start");
            } else {
                final String command = System.getenv().get("TERM");
                result.add(command);
                result.add("-e");
            }
        }

        String erl = r.getOtpHome() + "/bin/erl";
        if (erl.indexOf(' ') >= 0) {
            erl = "\"" + erl + "\"";
        }
        result.add(erl);
        for (final String path : r.getCodePath()) {
            if (!Strings.isNullOrEmpty(path)) {
                result.add("-pa");
                result.add(path);
            }
        }
        if (!useStartShell()) {
            result.add("-noshell");
        }

        if (!getNodeName().equals("")) {
            final String nameTag = isLongName() ? "-name" : "-sname";
            String nameOption = getNodeName();
            if (!nameOption.contains("@")) {
                nameOption += "@"
                        + HostnameUtils.getErlangHostName(isLongName());
            }
            result.add(nameTag);
            result.add(nameOption);
            final String cky = getCookie();
            if (!Strings.isNullOrEmpty(cky)) {
                result.add("-setcookie");
                result.add(cky);
            }
        }
        final String gotArgs = r.getArgs();
        if (!Strings.isNullOrEmpty(gotArgs)) {
            result.addAll(splitQuoted(gotArgs));
        }
        return result.toArray(new String[result.size()]);
    }

    private boolean hasDetachedConsole() {
        // TODO add GUI for "detached console"
        return "true".equals(System.getProperty("erlide.backend.detached"));
    }

    /**
     * split on spaces but respect quotes
     * 
     * @param theArgs
     * @return
     */
    private Collection<String> splitQuoted(final String theArgs) {
        final Pattern p = Pattern.compile("(\"[^\"]*?\"|'[^']*?'|\\S+)");
        final Matcher m = p.matcher(theArgs);
        final List<String> tokens = new ArrayList<String>();
        while (m.find()) {
            tokens.add(m.group(1));
        }
        return tokens;
    }

    public static boolean shouldManageNode(final String name) {
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

        final boolean isRunning = BackendCore.getBackendManager()
                .getEpmdWatcher().hasLocalNode(shortName);
        final boolean result = isLocal && !isRunning;
        return result;
    }
}

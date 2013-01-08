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

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
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
import org.eclipse.jdt.annotation.Nullable;
import org.erlide.core.model.IBeamLocator;
import org.erlide.core.model.erlang.ModuleKind;
import org.erlide.launch.ErlLaunchAttributes;
import org.erlide.launch.ErlangLaunchDelegate;
import org.erlide.launch.debug.ErlDebugConstants;
import org.erlide.runtime.HostnameUtils;
import org.erlide.runtime.InitialCall;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeInfoCatalog;
import org.erlide.utils.Asserts;
import org.erlide.utils.ErlLogger;
import org.erlide.utils.SystemConfiguration;

import com.google.common.base.Charsets;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public final class BackendData implements IBackendData {

    private RuntimeInfoCatalog runtimeInfoManager;
    private IBeamLocator beamLocator;
    private boolean fTransient = false;
    protected ILaunch launch;
    boolean debug = false;
    private String cookie;
    private boolean managed;
    private boolean restartable;
    private boolean startShell;
    private boolean console;
    private List<String> interpretedModules;
    private String runtimeName;
    private String nodeName;
    private boolean longName;
    private String extraArgs;
    private String workingDir;
    private Map<String, String> env;
    private InitialCall initialCall;
    private int debugFlags;
    private boolean loadOnAllNodes;
    private boolean internal;
    private Collection<IProject> projects;

    public BackendData(final RuntimeInfoCatalog runtimeInfoManager,
            final ILaunchConfiguration config, final String mode) {

        cookie = "";
        managed = true;
        restartable = false;
        startShell = true;
        console = true;
        interpretedModules = Lists.newArrayList();
        runtimeName = runtimeInfoManager.getDefaultRuntimeName();
        nodeName = "";
        longName = true;
        extraArgs = "";
        workingDir = ErlLaunchAttributes.DEFAULT_WORKING_DIR;
        env = Maps.newHashMap();
        initialCall = null;
        debugFlags = ErlDebugConstants.DEFAULT_DEBUG_FLAGS;
        loadOnAllNodes = false;
        internal = false;
        projects = Lists.newArrayList();
        interpretedModules = Lists.newArrayList();

        Asserts.isNotNull(config);
        try {
            init(config);
        } catch (final CoreException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }

        this.runtimeInfoManager = runtimeInfoManager;
        final RuntimeInfo runtimeInfo = runtimeInfoManager
                .getRuntime(getRuntimeName());
        if (runtimeInfo == null) {
            return;
        }
        try {
            if (config.getAttribute(ErlLaunchAttributes.EXTRA_ARGS, "").equals(
                    "")) {
                setExtraArgs(runtimeInfo.getArgs());
            }
        } catch (final CoreException e) {
        }
        setManaged(shouldManageNode(getNodeName()));
        debug = mode.equals(ILaunchManager.DEBUG_MODE);
    }

    @SuppressWarnings("unchecked")
    private void init(final ILaunchConfiguration config) throws CoreException {
        cookie = config.getAttribute(ErlLaunchAttributes.COOKIE, cookie);
        managed = config.getAttribute(ErlLaunchAttributes.MANAGED, managed);
        restartable = config.getAttribute(ErlLaunchAttributes.RESTARTABLE,
                restartable);
        startShell = config.getAttribute(ErlLaunchAttributes.SHELL, startShell);
        console = config.getAttribute(ErlLaunchAttributes.CONSOLE, console);

        runtimeName = config.getAttribute(ErlLaunchAttributes.RUNTIME_NAME,
                runtimeName);
        nodeName = config.getAttribute(ErlLaunchAttributes.NODE_NAME, nodeName);
        longName = config.getAttribute(ErlLaunchAttributes.USE_LONG_NAME,
                longName);
        extraArgs = config.getAttribute(ErlLaunchAttributes.EXTRA_ARGS,
                extraArgs);
        workingDir = config.getAttribute(ErlLaunchAttributes.WORKING_DIR,
                workingDir);
        env = config.getAttribute(ILaunchManager.ATTR_ENVIRONMENT_VARIABLES,
                env);
        initialCall = getInitialCall(config);
        debugFlags = config.getAttribute(ErlLaunchAttributes.DEBUG_FLAGS,
                debugFlags);
        loadOnAllNodes = config.getAttribute(
                ErlLaunchAttributes.LOAD_ALL_NODES, loadOnAllNodes);
        internal = config.getAttribute(ErlLaunchAttributes.INTERNAL, internal);

        projects = getProjects(config);
        final List<String> intMods = config
                .getAttribute(ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
                        interpretedModules);
        interpretedModules = addBreakpointProjectsAndModules(getProjects(),
                intMods);

    }

    public BackendData(final RuntimeInfoCatalog runtimeInfoManager,
            final RuntimeInfo info) {
        cookie = "";
        managed = true;
        restartable = false;
        startShell = true;
        console = true;
        interpretedModules = Lists.newArrayList();
        runtimeName = runtimeInfoManager.getDefaultRuntimeName();
        nodeName = "";
        longName = true;
        extraArgs = "";
        workingDir = ErlLaunchAttributes.DEFAULT_WORKING_DIR;
        env = Maps.newHashMap();
        initialCall = null;
        debugFlags = ErlDebugConstants.DEFAULT_DEBUG_FLAGS;
        loadOnAllNodes = false;
        internal = false;
        projects = Lists.newArrayList();
        interpretedModules = Lists.newArrayList();

        Asserts.isNotNull(info, "Can't create backend with no runtime info");

        this.runtimeInfoManager = runtimeInfoManager;
        setRuntimeName(info.getName());
        setCookie("erlide");
        setLongName(true);

        setWorkingDir(getDefaultWorkingDir());
        setExtraArgs(info.getArgs());

        setConsole(true);
        setLoadAllNodes(false);
        debug = false;
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

    public static List<String> addBreakpointProjectsAndModules(
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

    public void setLaunch(final ILaunch launch) {
        this.launch = launch;
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
        return cookie;
    }

    @Override
    public void setCookie(final String cookie) {
        this.cookie = cookie.trim();
    }

    @Override
    public boolean isManaged() {
        return managed;
    }

    @Override
    public void setManaged(final boolean managed) {
        this.managed = managed;
    }

    @Override
    public boolean isRestartable() {
        return restartable;
    }

    @Override
    public void setRestartable(final boolean restartable) {
        this.restartable = restartable;
    }

    @Override
    public boolean useStartShell() {
        return startShell;
    }

    @Override
    public void setUseStartShell(final boolean shell) {
        this.startShell = shell;
    }

    @Override
    public boolean hasConsole() {
        return console;
    }

    @Override
    public void setConsole(final boolean console) {
        this.console = console;
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
    public List<String> getInterpretedModules() {
        return interpretedModules;
    }

    @Override
    public void setInterpretedModules(final List<String> interpretedModules) {
        this.interpretedModules = interpretedModules;
    }

    @Override
    public String getRuntimeName() {
        return runtimeName;
    }

    @Override
    public void setRuntimeName(final String name) {
        this.runtimeName = name;
    }

    @Override
    public String getNodeName() {
        return nodeName;
    }

    @Override
    public void setNodeName(String nodeName) {
        if (!validateNodeName(nodeName)) {
            // TODO this still can create a name that isn't valid
            nodeName = nodeName.replaceAll("[^a-zA-Z0-9_-]", "");
        }
        this.nodeName = nodeName;
    }

    public static boolean validateNodeName(final String name) {
        return name != null
                && name.matches("[a-zA-Z0-9_-]+(@[a-zA-Z0-9_.-]+)?");
    }

    @Override
    public boolean isLongName() {
        return longName;
    }

    @Override
    public void setLongName(final boolean longname) {
        this.longName = longname;
    }

    @Override
    public String getExtraArgs() {
        return extraArgs;
    }

    @Override
    public void setExtraArgs(final String xtra) {
        this.extraArgs = xtra;
    }

    @Override
    public String getWorkingDir() {
        return workingDir;
    }

    @Override
    public void setWorkingDir(final String dir) {
        this.workingDir = dir;
    }

    @Override
    public Map<String, String> getEnv() {
        return env;
    }

    private InitialCall getInitialCall(final ILaunchConfiguration config)
            throws CoreException {
        final String module = config.getAttribute(ErlLaunchAttributes.MODULE,
                "");
        final String function = config.getAttribute(
                ErlLaunchAttributes.FUNCTION, "");
        final String args = config.getAttribute(ErlLaunchAttributes.ARGUMENTS,
                "");
        return new InitialCall(module, function, args);
    }

    @Override
    @Nullable
    public InitialCall getInitialCall() {
        return initialCall;
    }

    @Override
    public Collection<IProject> getProjects() {
        return projects;
    }

    private Collection<IProject> getProjects(final ILaunchConfiguration config)
            throws CoreException {
        String prjs;
        prjs = config.getAttribute(ErlLaunchAttributes.PROJECTS, "");
        final String[] projectNames = prjs.length() == 0 ? new String[] {}
                : prjs.split(PROJECT_NAME_SEPARATOR);
        return gatherProjects(projectNames);
    }

    @Override
    public int getDebugFlags() {
        return debugFlags;
    }

    @Override
    public boolean shouldLoadOnAllNodes() {
        return loadOnAllNodes;
    }

    @Override
    public void setLoadAllNodes(final boolean load) {
        this.loadOnAllNodes = load;
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
        return internal;
    }

    @Override
    public void setInternal(final boolean internal) {
        this.internal = internal;
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

        final boolean isRunning = BackendCore.getEpmdWatcher().hasLocalNode(
                shortName);
        final boolean result = isLocal && !isRunning;
        return result;
    }

    @Override
    public String getQualifiedNodeName() {
        final String erlangHostName = HostnameUtils
                .getErlangHostName(isLongName());
        final String nodeName = getNodeName();
        final boolean hasHost = nodeName.contains("@");
        return hasHost ? nodeName : nodeName + "@" + erlangHostName;
    }

    public void debugPrint() {
        final String mode = debug ? "debug" : "run";
        ErlLogger.info("Backend data:" + mode + " mode,  with attributes::");
        final Class<? extends BackendData> clazz = this.getClass();
        try {
            for (final Field field : clazz.getDeclaredFields()) {
                ErlLogger.info("%-20s: %s", field.getName(), field.get(this));
            }
        } catch (final IllegalArgumentException e) {
            ErlLogger.info("Could not get attributes! %s", e.getMessage());
        } catch (final IllegalAccessException e) {
            ErlLogger.info("Could not get attributes! %s", e.getMessage());
        }
        ErlLogger.info("---------------");
    }

}

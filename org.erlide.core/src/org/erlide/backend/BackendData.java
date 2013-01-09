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

import java.util.Collection;
import java.util.List;

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
import org.erlide.runtime.ErlDebugFlags;
import org.erlide.runtime.HostnameUtils;
import org.erlide.runtime.InitialCall;
import org.erlide.runtime.RuntimeData;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.utils.Asserts;
import org.erlide.utils.ErlLogger;

import com.google.common.base.Charsets;
import com.google.common.collect.Lists;

public final class BackendData extends RuntimeData {

    public static final String PROJECT_NAME_SEPARATOR = ";";

    private IBeamLocator beamLocator;
    protected ILaunch launch;
    private Collection<IProject> projects;

    public BackendData() {
        super();
        projects = Lists.newArrayList();
    }

    @SuppressWarnings("unchecked")
    public BackendData(final RuntimeInfo runtime,
            final ILaunchConfiguration config, final String mode) {
        super(runtime, mode);

        Asserts.isNotNull(config);
        try {
            cookie = config.getAttribute(ErlLaunchAttributes.COOKIE, cookie);
            managed = config.getAttribute(ErlLaunchAttributes.MANAGED, managed);
            restartable = config.getAttribute(ErlLaunchAttributes.RESTARTABLE,
                    restartable);
            startShell = config.getAttribute(ErlLaunchAttributes.SHELL,
                    startShell);
            console = config.getAttribute(ErlLaunchAttributes.CONSOLE, console);

            runtimeName = config.getAttribute(ErlLaunchAttributes.RUNTIME_NAME,
                    runtimeName);
            nodeName = config.getAttribute(ErlLaunchAttributes.NODE_NAME,
                    nodeName);
            longName = config.getAttribute(ErlLaunchAttributes.USE_LONG_NAME,
                    longName);
            extraArgs = config.getAttribute(ErlLaunchAttributes.EXTRA_ARGS,
                    extraArgs);
            workingDir = config.getAttribute(ErlLaunchAttributes.WORKING_DIR,
                    workingDir);
            env = config.getAttribute(
                    ILaunchManager.ATTR_ENVIRONMENT_VARIABLES, env);
            initialCall = createInitialCall(config);
            debugFlags = ErlDebugFlags.makeSet(config.getAttribute(
                    ErlLaunchAttributes.DEBUG_FLAGS,
                    ErlDebugFlags.getFlag(debugFlags)));
            loadOnAllNodes = config.getAttribute(
                    ErlLaunchAttributes.LOAD_ALL_NODES, loadOnAllNodes);
            internal = config.getAttribute(ErlLaunchAttributes.INTERNAL,
                    internal);

            projects = getProjects(config);
            final List<String> intMods = config.getAttribute(
                    ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
                    interpretedModules);
            interpretedModules = addBreakpointProjectsAndModules(getProjects(),
                    intMods);
        } catch (final CoreException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
        projects = Lists.newArrayList();

        if (runtime != null) {
            runtimeInfo = runtime.setArgs(getExtraArgs());
            runtimeName = runtime.getName();
            try {
                if (config.getAttribute(ErlLaunchAttributes.EXTRA_ARGS, "")
                        .equals("")) {
                    setExtraArgs(runtime.getArgs());
                }
            } catch (final CoreException e) {
            }
        }
        setManaged(shouldManageNode(getNodeName(), BackendCore.getEpmdWatcher()));
    }

    public BackendData(final RuntimeInfo info) {
        super(info, "run", getDefaultWorkingDir());
        projects = Lists.newArrayList();
    }

    private static String getDefaultWorkingDir() {
        final IWorkspaceRoot wroot = ResourcesPlugin.getWorkspace().getRoot();
        return wroot.getLocation().toPortableString();
    }

    private List<IProject> gatherProjects(final String[] projectNames) {
        final List<IProject> myProjects = Lists.newArrayList();
        for (final String s : projectNames) {
            final IProject project = ResourcesPlugin.getWorkspace().getRoot()
                    .getProject(s);
            if (project == null) {
                ErlLogger.error("Launch: project not found: '%s'!", s);
                continue;
            }
            myProjects.add(project);
        }
        return myProjects;
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

    public ILaunch getLaunch() {
        return launch;
    }

    public void setLaunch(final ILaunch launch) {
        this.launch = launch;
    }

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
                    hasLongName());
            workingCopy
                    .setAttribute(ErlLaunchAttributes.INTERNAL, isInternal());

            return workingCopy;
        } catch (final CoreException e) {
            e.printStackTrace();
            return null;
        }
    }

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

    public void setBeamLocator(final IBeamLocator beamLocator) {
        this.beamLocator = beamLocator;
    }

    public IBeamLocator getBeamLocator() {
        return beamLocator;
    }

    @Override
    public String getQualifiedNodeName() {
        final String erlangHostName = HostnameUtils
                .getErlangHostName(hasLongName());
        final String name = getNodeName();
        final boolean hasHost = name.contains("@");
        return hasHost ? name : name + "@" + erlangHostName;
    }

    private InitialCall createInitialCall(final ILaunchConfiguration config)
            throws CoreException {
        final String module = config.getAttribute(ErlLaunchAttributes.MODULE,
                "");
        final String function = config.getAttribute(
                ErlLaunchAttributes.FUNCTION, "");
        final String args = config.getAttribute(ErlLaunchAttributes.ARGUMENTS,
                "");
        return new InitialCall(module, function, args);
    }

}

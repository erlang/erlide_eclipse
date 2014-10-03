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
package org.erlide.backend.api;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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
import org.eclipse.jdt.annotation.NonNull;
import org.erlide.backend.api.ICodeBundle.CodeContext;
import org.erlide.backend.debug.ErlDebugConstants;
import org.erlide.backend.launch.IErlangLaunchDelegateConstants;
import org.erlide.engine.model.IBeamLocator;
import org.erlide.engine.model.erlang.SourceKind;
import org.erlide.runtime.api.ErlDebugFlags;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.ErlLogger;
import org.erlide.util.ErlangFunctionCall;
import org.erlide.util.HostnameUtils;

import com.google.common.base.Charsets;
import com.google.common.collect.Lists;

public final class BackendData extends RuntimeData {

    public static final String PROJECT_NAME_SEPARATOR = ";";

    private IBeamLocator beamLocator;
    protected ILaunch launch;
    private Collection<IProject> projects;
    private CodeContext context = CodeContext.COMMON;

    public BackendData(@NonNull final RuntimeInfo info,
            @NonNull final ILaunchConfiguration config, final String mode,
            final boolean toBeManaged) {
        super(info, mode);
        projects = Lists.newArrayList();
        try {
            cookie = config.getAttribute(ErlRuntimeAttributes.COOKIE, cookie);
            managed = config.getAttribute(ErlRuntimeAttributes.MANAGED, managed);
            restartable = config.getAttribute(ErlRuntimeAttributes.RESTARTABLE,
                    restartable);
            startShell = config.getAttribute(ErlRuntimeAttributes.SHELL, startShell);
            console = config.getAttribute(ErlRuntimeAttributes.CONSOLE, console);

            nodeName = config.getAttribute(ErlRuntimeAttributes.NODE_NAME, nodeName);
            longName = config.getAttribute(ErlRuntimeAttributes.USE_LONG_NAME, longName);
            extraArgs = config.getAttribute(ErlRuntimeAttributes.EXTRA_ARGS, extraArgs);
            workingDir = config
                    .getAttribute(ErlRuntimeAttributes.WORKING_DIR, workingDir);
            env = config.getAttribute(ILaunchManager.ATTR_ENVIRONMENT_VARIABLES, env);
            initialCall = createInitialCall(config);
            debugFlags = ErlDebugFlags.makeSet(config.getAttribute(
                    ErlRuntimeAttributes.DEBUG_FLAGS, ErlDebugFlags.getFlag(debugFlags)));
            loadOnAllNodes = config.getAttribute(ErlRuntimeAttributes.LOAD_ALL_NODES,
                    loadOnAllNodes);
            internal = config.getAttribute(ErlRuntimeAttributes.INTERNAL, internal);

            projects = getProjects(config);
            final List<String> defList = Lists.newArrayList();
            final List<String> intMods = config.getAttribute(
                    ErlRuntimeAttributes.DEBUG_INTERPRET_MODULES, defList);
            initialInterpretedModules = addBreakpointProjectsAndModules(getProjects(),
                    intMods);
        } catch (final CoreException e1) {
            ErlLogger.warn(e1);
        }

        if (extraArgs != null) {
            runtimeInfo = new RuntimeInfo.Builder(info).withArgs(extraArgs).build();
        }
        try {
            if (config.getAttribute(ErlRuntimeAttributes.EXTRA_ARGS, "").equals("")) {
                setExtraArgs(info.getArgs());
            }
        } catch (final CoreException e) {
        }
        setManaged(toBeManaged);
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

    public static Set<String> addBreakpointProjectsAndModules(
            final Collection<IProject> projects, final List<String> interpretedModules) {
        final IBreakpointManager bpm = DebugPlugin.getDefault().getBreakpointManager();
        final Set<String> result = new HashSet<String>(interpretedModules);
        for (final IBreakpoint bp : bpm
                .getBreakpoints(ErlDebugConstants.ID_ERLANG_DEBUG_MODEL)) {
            final IMarker m = bp.getMarker();
            final IResource r = m.getResource();
            final String name = r.getName();
            if (SourceKind.hasErlExtension(name)) {
                final IProject p = r.getProject();
                if (projects == null || projects.contains(p)) {
                    final String s = p.getName() + ":" + name;
                    result.add(s);
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
        final ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
        final ILaunchConfigurationType type = manager
                .getLaunchConfigurationType(IErlangLaunchDelegateConstants.CONFIGURATION_TYPE_INTERNAL);
        ILaunchConfigurationWorkingCopy workingCopy;
        try {
            final RuntimeInfo info = getRuntimeInfo();
            final String name = getNodeName();
            workingCopy = type.newInstance(null, name);
            if (info.getVersion().isReleaseCompatible(new RuntimeVersion(17))) {
                workingCopy.setAttribute(DebugPlugin.ATTR_CONSOLE_ENCODING,
                        Charsets.UTF_8.name());
            } else {
                workingCopy.setAttribute(DebugPlugin.ATTR_CONSOLE_ENCODING,
                        Charsets.ISO_8859_1.name());
            }

            workingCopy.setAttribute(DebugPlugin.ATTR_PROCESS_FACTORY_ID,
                    "org.erlide.backend.ertsProcessFactory");

            workingCopy.setAttribute(ErlRuntimeAttributes.NODE_NAME, getNodeName());
            workingCopy.setAttribute(ErlRuntimeAttributes.RUNTIME_NAME, info.getName());
            workingCopy.setAttribute(ErlRuntimeAttributes.COOKIE, getCookie());
            // workingCopy.setAttribute(ErlLaunchAttributes.CONSOLE,
            // !options.contains(BackendOptions.NO_CONSOLE));
            workingCopy.setAttribute(ErlRuntimeAttributes.USE_LONG_NAME, hasLongName());
            workingCopy.setAttribute(ErlRuntimeAttributes.INTERNAL, isInternal());

            return workingCopy;
        } catch (final CoreException e) {
            ErlLogger.error(e);
            return null;
        }
    }

    public Collection<IProject> getProjects() {
        return projects;
    }

    private Collection<IProject> getProjects(final ILaunchConfiguration config)
            throws CoreException {
        String prjs;
        prjs = config.getAttribute(ErlRuntimeAttributes.PROJECTS, "");
        final String[] projectNames = prjs.length() == 0 ? new String[] {} : prjs
                .split(PROJECT_NAME_SEPARATOR);
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
        final String erlangHostName = HostnameUtils.getErlangHostName(hasLongName());
        final String name = getNodeName();
        final boolean hasHost = name.contains("@");
        return hasHost ? name : name + "@" + erlangHostName;
    }

    private ErlangFunctionCall createInitialCall(final ILaunchConfiguration config)
            throws CoreException {
        final String module = config.getAttribute(ErlRuntimeAttributes.MODULE, "");
        final String function = config.getAttribute(ErlRuntimeAttributes.FUNCTION, "");
        final String args = config.getAttribute(ErlRuntimeAttributes.ARGUMENTS, "");
        return new ErlangFunctionCall(module, function, args);
    }

    public CodeContext getContext() {
        return context;
    }

    public void setContext(final CodeContext context) {
        this.context = context;
    }
}

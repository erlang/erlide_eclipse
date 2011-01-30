/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *     Jakob Cederlund
 *******************************************************************************/
package org.erlide.runtime.launch;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.RegistryFactory;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IBreakpointManager;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.BackendManager.BackendOptions;
import org.erlide.runtime.backend.ErlideBackend;
import org.erlide.runtime.debug.ErlDebugConstants;
import org.erlide.runtime.debug.ErlangDebugNode;
import org.erlide.runtime.debug.ErlangDebugTarget;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Maps;

import erlang.ErlideDebug;

public class ErlangLaunchDelegate implements
        ILaunchConfigurationDelegate {

    private ErlangDebugTarget target;

    public void launch(final ILaunchConfiguration config, final String mode,
            final ILaunch launch, final IProgressMonitor monitor)
            throws CoreException {
        doLaunch(config, mode, launch, false, null);
    }

    protected void doLaunch(final ILaunchConfiguration config,
            final String mode, final ILaunch launch, final boolean internal,
            final Map<String, String> env) throws CoreException {
        final ErlLaunchData data = new ErlLaunchData(config, internal);

        if (data.isInternal) {
            return;
        }

        final Set<IProject> projects = gatherProjects(data);
        data.interpretedModules = addBreakpointProjectsAndModules(projects,
                data.interpretedModules);

        // if (true || ErlideUtil.isDeveloper()) {
        data.debugPrint(mode);
        // }

        final RuntimeInfo rt0 = ErlangCore.getRuntimeInfoManager().getRuntime(
                data.runtime);
        if (rt0 == null) {
            ErlLogger.error("Could not find runtime %s", data.runtime);
            return;
        }
        final RuntimeInfo rt = buildRuntimeInfo(internal, data, rt0);
        final EnumSet<BackendOptions> options = setupBackendOptions(mode, data);
        Map<String, String> myenv = setupEnvironment(env, data);
        setCaptureOutput(launch);

        ErlideBackend backend = null;
        try {
            backend = ErlangCore.getBackendManager().createBackend(rt, options,
                    launch, myenv);
            if (backend == null) {
                ErlLogger.error("Launch: could not create backend!");
                final Status s = new Status(IStatus.ERROR,
                        ErlangPlugin.PLUGIN_ID, DebugException.REQUEST_FAILED,
                        "Couldn't find the node " + data.nodeName, null);
                throw new DebugException(s);
            }
            postLaunch(mode, data, projects, rt, options, backend);
        } catch (final BackendException e) {
            ErlLogger.error("Launch: backend error!");
            final Status s = new Status(IStatus.ERROR, ErlangPlugin.PLUGIN_ID,
                    DebugException.REQUEST_FAILED, e.getMessage(), null);
            throw new DebugException(s);
        }
    }

    private EnumSet<BackendOptions> setupBackendOptions(final String mode,
            final ErlLaunchData data) throws CoreException {
        final EnumSet<BackendOptions> options = EnumSet
                .noneOf(BackendOptions.class);
        if (mode.equals(ILaunchManager.DEBUG_MODE)) {
            options.add(BackendOptions.DEBUG);
            if ("".equals(data.nodeName)) {
                throw new CoreException(
                        new Status(
                                IStatus.ERROR,
                                ErlangPlugin.PLUGIN_ID,
                                "A node name has to be specified in the "
                                        + "configuration dialog when launching in debug mode!"));
            }
        }
        if (data.startMe) {
            options.add(BackendOptions.AUTOSTART);
        }
        if (!data.console) {
            options.add(BackendOptions.NO_CONSOLE);
        }
        if (data.loadAllNodes) {
            options.add(BackendOptions.LOAD_ALL_NODES);
        }
        return options;
    }

    private HashMap<String, String> setupEnvironment(
            final Map<String, String> env, final ErlLaunchData data) {
        HashMap<String, String> myenv;
        if (env == null) {
            myenv = Maps.newHashMap();
        } else {
            myenv = Maps.newHashMap(env);
        }
        myenv.putAll(data.env);
        return myenv;
    }

    private void setCaptureOutput(final ILaunch launch) {
        // important, so that we don't get the "normal" console for the erlide
        // backend
        final String captureOutput = System.getProperty(
                "erlide.console.stdout", "false");
        launch.setAttribute(DebugPlugin.ATTR_CAPTURE_OUTPUT, captureOutput);
    }

    private Set<IProject> gatherProjects(final ErlLaunchData data) {
        final Set<IProject> projects = new HashSet<IProject>();
        for (final String s : data.projectNames) {
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

    private RuntimeInfo buildRuntimeInfo(final boolean internal,
            final ErlLaunchData data, final RuntimeInfo rt0) {
        final RuntimeInfo rt = RuntimeInfo.copy(rt0, false);
        rt.setNodeName(data.nodeName);
        if (internal) {
            rt.setCookie("erlide");
        } else {
            rt.setCookie(data.cookie);
        }

        rt.setStartShell(true);
        final File d = new File(data.workingDir);
        if (d.isAbsolute()) {
            rt.setWorkingDir(data.workingDir);
        } else {
            final String wspace = ResourcesPlugin.getWorkspace().getRoot()
                    .getLocation().toPortableString();
            rt.setWorkingDir(wspace + "/" + data.workingDir);
        }
        rt.setArgs(data.xtraArgs);
        rt.useLongName(data.longName);
        rt.setHasConsole(data.console);
        rt.setLoadAllNodes(data.loadAllNodes);
        return rt;
    }

    protected void postLaunch(final String mode, final ErlLaunchData data,
            final Set<IProject> projects, final RuntimeInfo rt,
            final EnumSet<BackendOptions> options, final ErlideBackend backend)
            throws DebugException {

        registerProjectsWithExecutionBackend(backend, projects);
        if (!backend.isDistributed()) {
            return;
        }
        if (mode.equals(ILaunchManager.DEBUG_MODE)) {
            final ILaunch launch = backend.getLaunch();
            // add debug target
            target = new ErlangDebugTarget(launch, backend, projects,
                    data.debugFlags);
            // target.getWaiter().doWait();
            launch.addDebugTarget(target);
            // interpret everything we can
            final boolean distributed = (data.debugFlags & ErlDebugConstants.DISTRIBUTED_DEBUG) != 0;
            if (distributed) {
                distributeDebuggerCode(backend);
                addNodesAsDebugTargets(backend, launch);
            }
            interpretModules(data, backend, distributed);
            registerStartupFunctionStarter(data, backend);
            target.sendStarted();
        } else {
            runInitial(data.module, data.function, data.args, backend);
        }
    }

    private void interpretModules(final ErlLaunchData data,
            final ErlideBackend backend, final boolean distributed) {
        for (final String pm : data.interpretedModules) {
            final String[] pms = pm.split(":");
            new ErlangDebugHelper().interpret(backend, pms[0], pms[1],
                    distributed, true);
        }
    }

    private void addNodesAsDebugTargets(final Backend backend,
            final ILaunch launch) {
        final OtpErlangList nodes = ErlideDebug.nodes(backend);
        if (nodes != null) {
            for (int i = 1, n = nodes.arity(); i < n; ++i) {
                final OtpErlangAtom o = (OtpErlangAtom) nodes.elementAt(i);
                final OtpErlangAtom a = o;
                final ErlangDebugNode edn = new ErlangDebugNode(target,
                        a.atomValue());
                launch.addDebugTarget(edn);
            }
        }
    }

    private void registerStartupFunctionStarter(final ErlLaunchData data,
            final ErlideBackend backend) {
        DebugPlugin.getDefault().addDebugEventListener(
                new IDebugEventSetListener() {
                    public void handleDebugEvents(final DebugEvent[] events) {
                        runInitial(data.module, data.function, data.args,
                                backend);
                        DebugPlugin.getDefault().removeDebugEventListener(this);
                    }
                });
    }

    private static void registerProjectsWithExecutionBackend(
            final ErlideBackend backend, final Collection<IProject> projects) {
        for (final IProject project : projects) {
            ErlangCore.getBackendManager()
                    .addExecutionBackend(project, backend);
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
            if (ErlideUtil.hasErlExtension(name)) {
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

    private void distributeDebuggerCode(final Backend backend) {
        final String[] debuggerModules = { "erlide_dbg_debugged",
                "erlide_dbg_icmd", "erlide_dbg_idb", "erlide_dbg_ieval",
                "erlide_dbg_iload", "erlide_dbg_iserver", "erlide_int", "int" };
        final List<OtpErlangTuple> modules = new ArrayList<OtpErlangTuple>(
                debuggerModules.length);
        for (final String module : debuggerModules) {
            final OtpErlangBinary b = getBeam(module, backend);
            if (b != null) {
                final OtpErlangString filename = new OtpErlangString(module
                        + ".erl");
                final OtpErlangTuple t = OtpErlang.mkTuple(new OtpErlangAtom(
                        module), filename, b);
                modules.add(t);
            }
        }
        ErlideDebug.distributeDebuggerCode(backend, modules);
    }

    /**
     * Get a named beam-file as a binary from the core plug-in bundle
     * 
     * @param module
     *            module name, without extension
     * @param backend
     *            the execution backend
     * @return
     */
    private OtpErlangBinary getBeam(final String module, final Backend backend) {
        final Bundle b = Platform.getBundle("org.erlide.kernel.debugger");
        final String beamname = module + ".beam";
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] els = reg.getConfigurationElementsFor(
                ErlangPlugin.PLUGIN_ID, "codepath");
        // TODO: this code assumes that the debugged target and the
        // erlide-plugin uses the same Erlang version, how can we escape this?
        final String ver = backend.getCurrentVersion();
        for (final IConfigurationElement el : els) {
            final IContributor c = el.getContributor();
            if (c.getName().equals(b.getSymbolicName())) {
                final String dir_path = el.getAttribute("path");
                Enumeration<?> e = b.getEntryPaths(dir_path + "/" + ver);
                if (e == null || !e.hasMoreElements()) {
                    e = b.getEntryPaths(dir_path);
                }
                if (e == null) {
                    ErlLogger.debug("* !!! error loading plugin "
                            + b.getSymbolicName());
                    return null;
                }
                while (e.hasMoreElements()) {
                    final String s = (String) e.nextElement();
                    final Path path = new Path(s);
                    if (path.lastSegment().equals(beamname)) {
                        if (path.getFileExtension() != null
                                && "beam".compareTo(path.getFileExtension()) == 0) {
                            final String m = path.removeFileExtension()
                                    .lastSegment();
                            try {
                                return ErlideUtil.getBeamBinary(m,
                                        b.getEntry(s));
                            } catch (final Exception ex) {
                                ErlLogger.warn(ex);
                            }
                        }
                    }
                }
            }
        }
        return null;
    }

    void runInitial(final String module, final String function,
            final String args, final Backend backend) {
        ErlLogger.debug("calling startup function %s:%s", module, function);
        try {
            if (module.length() > 0 && function.length() > 0) {
                if (args.length() > 0) {
                    backend.cast(module, function, "s", args);
                } else {
                    backend.cast(module, function, "");
                }
            }
        } catch (final Exception e) {
            ErlLogger.debug("Could not run initial call %s:%s(\"%s\")", module,
                    function, args);
            ErlLogger.warn(e);
        }
    }

    /**
     * used by selfhost plugin
     */
    public void launchInternal(final ILaunchConfiguration configuration,
            final String mode, final ILaunch launch,
            final IProgressMonitor monitor) throws CoreException {
        doLaunch(configuration, mode, launch, true, null);
    }

    public ErlangDebugTarget getDebugTarget() {
        return target;
    }
}

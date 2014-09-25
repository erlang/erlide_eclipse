/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend.debug.model;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.RegistryFactory;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IMemoryBlock;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IThread;
import org.eclipse.jdt.annotation.NonNull;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.debug.BeamUtil;
import org.erlide.backend.debug.DebuggerEventDaemon;
import org.erlide.backend.debug.ErlangLineBreakpoint;
import org.erlide.backend.debug.ErlideDebug;
import org.erlide.backend.debug.IErlangDebugNode;
import org.erlide.backend.internal.BackendActivator;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.runtime.api.ErlDebugFlags;
import org.erlide.util.ErlLogger;
import org.erlide.util.IDisposable;
import org.erlide.util.erlang.ErlUtils;
import org.erlide.util.erlang.OtpErlang;
import org.erlide.util.erlang.SignatureException;
import org.erlide.util.erlang.TermParserException;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class ErlangDebugTarget extends ErlangDebugElement implements IDebugTarget,
        IErlangDebugNode, IDisposable {

    private static final OtpErlangAtom PARENT_ATOM = new OtpErlangAtom("parent");

    public static final IThread[] NO_PROCS = new IThread[] {};

    public static final int INTERPRETED_MODULES_CHANGED = 0;
    public static final int TRACE_CHANGED = 1;

    private final List<ErlangProcess> allProcesses;
    private final List<ErlangProcess> localProcesses;
    final IBackend backend;
    private final ILaunch launch;
    private boolean disconnected = false;
    // private final DebuggerListener fDbgListener;
    // private final DebuggerEventListener fDebuggerEventListener;
    private boolean terminated = false;
    private boolean showSystemProcesses = false;
    private boolean showErlideProcesses = false;
    private final Set<String> interpretedModules;
    private final Collection<IProject> projects;

    private final Map<OtpErlangPid, OtpErlangPid> metaPids = new TreeMap<OtpErlangPid, OtpErlangPid>();
    private final Map<OtpErlangPid, OtpErlangPid> pidsFromMeta = new TreeMap<OtpErlangPid, OtpErlangPid>();

    private final DebuggerEventDaemon debuggerDaemon;
    private boolean disposed = false;

    public ErlangDebugTarget(final ILaunch launch, final IBackend backend,
            final Collection<IProject> projects) throws DebugException {
        super(null);
        this.backend = backend;
        this.launch = launch;
        this.projects = projects;

        allProcesses = new ArrayList<ErlangProcess>();
        localProcesses = new ArrayList<ErlangProcess>();
        interpretedModules = new HashSet<String>();

        debuggerDaemon = new DebuggerEventDaemon(backend, this);
        debuggerDaemon.start();

        // interpret everything we can
        final EnumSet<ErlDebugFlags> debugFlags = backend.getData().getDebugFlags();
        final boolean distributed = debugFlags.contains(ErlDebugFlags.DISTRIBUTED_DEBUG);
        if (distributed) {
            distributeDebuggerCode();
            addNodesAsDebugTargets(launch);
        }

        final OtpErlangPid pid = ErlideDebug.startDebug(backend.getOtpRpc(),
                ErlDebugFlags.getFlag(debugFlags));
        ErlLogger.debug("debug started " + pid);
        backend.getOtpRpc().send(pid,
                OtpErlang.mkTuple(PARENT_ATOM, debuggerDaemon.getMBox()));

        DebugPlugin.getDefault().getBreakpointManager().addBreakpointListener(this);

        interpretModules(backend.getData().getInitialInterpretedModules(), distributed);
    }

    @Override
    public ILaunch getLaunch() {
        return launch;
    }

    @Override
    public IDebugTarget getDebugTarget() {
        return this;
    }

    @Override
    public IProcess getProcess() {
        return null;
    }

    @Override
    public IThread[] getThreads() throws DebugException {
        if (isTerminated()) {
            return NO_PROCS;
        }
        return localProcesses.toArray(new IThread[localProcesses.size()]);
    }

    @Override
    public boolean hasThreads() throws DebugException {
        return !isTerminated();
    }

    @Override
    public String getName() throws DebugException {
        return backend.getName();
    }

    @Override
    public boolean supportsBreakpoint(final IBreakpoint breakpoint) {
        // TODO we should ask the Erlang debugger too...
        if (!isTerminated()
                && breakpoint.getModelIdentifier().equals(getModelIdentifier())) {
            final IProject bpProject = breakpoint.getMarker().getResource().getProject();
            for (final IProject p : projects) {
                if (p == bpProject) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public boolean canTerminate() {
        return true;
    }

    @Override
    public boolean isTerminated() {
        return terminated;
    }

    @Override
    public void terminate() throws DebugException {
        if (terminated) {
            return;
        }
        terminated = true;

        if (backend != null) {
            if (backend.getOtpRpc() != null) {
                backend.getOtpRpc().send("dbg_mon", new OtpErlangAtom("stop"));
            }
            final DebugPlugin dbgPlugin = DebugPlugin.getDefault();
            if (dbgPlugin != null) {
                dbgPlugin.getBreakpointManager().removeBreakpointListener(this);
            }
            if (debuggerDaemon != null) {
                debuggerDaemon.stop();
            }
            backend.dispose();
        }
        if (launch != null) {
            launch.terminate();
        }

        fireTerminateEvent();
    }

    /**
     * Notification we have connected to the VM and it has started. Resume the
     * VM.
     */
    public void started() {
        fireCreationEvent();
        installDeferredBreakpoints();
        try {
            resume();
        } catch (final DebugException e) {
            ErlLogger.warn(e);
        }
    }

    /**
     * Install breakpoints that are already registered with the breakpoint
     * manager.
     */
    public void installDeferredBreakpoints() {
        final IBreakpoint[] breakpoints = DebugPlugin.getDefault().getBreakpointManager()
                .getBreakpoints(getModelIdentifier());
        for (int i = 0; i < breakpoints.length; i++) {
            breakpointAdded(breakpoints[i]);
        }
    }

    @Override
    public boolean canResume() {
        return false;
    }

    @Override
    public boolean canSuspend() {
        return false;
    }

    @Override
    public boolean isSuspended() {
        return false;
    }

    @Override
    public void resume() throws DebugException {
    }

    @Override
    public void suspend() throws DebugException {
    }

    @Override
    public void breakpointAdded(final IBreakpoint breakpoint) {
        if (supportsBreakpoint(breakpoint)) {
            try {
                if (breakpoint.isEnabled()
                        && DebugPlugin.getDefault().getBreakpointManager().isEnabled()
                        || !breakpoint.isRegistered()) {
                    final ErlangLineBreakpoint erlangLineBreakpoint = (ErlangLineBreakpoint) breakpoint;
                    erlangLineBreakpoint.install(this);
                }
            } catch (final CoreException e) {
                ErlLogger.error(e);
            }
        }

    }

    @Override
    public void breakpointRemoved(final IBreakpoint breakpoint, final IMarkerDelta delta) {
        try {
            ErlLogger.debug("breakpointRemoved " + breakpoint.getMarker().toString()
                    + breakpoint.getMarker().getAttribute(IMarker.LINE_NUMBER));
        } catch (final CoreException e) {
        }
        if (supportsBreakpoint(breakpoint)) {
            final ErlangLineBreakpoint erlangLineBreakpoint = (ErlangLineBreakpoint) breakpoint;
            erlangLineBreakpoint.remove(this);
        }
    }

    @Override
    public void breakpointChanged(final IBreakpoint breakpoint, final IMarkerDelta delta) {
        if (supportsBreakpoint(breakpoint)) {
            try {
                if (breakpoint.isEnabled()
                        && DebugPlugin.getDefault().getBreakpointManager().isEnabled()) {
                    breakpointAdded(breakpoint);
                } else {
                    breakpointRemoved(breakpoint, null);
                }
            } catch (final CoreException e) {
                ErlLogger.warn(e);
            }
        }
    }

    @Override
    public boolean canDisconnect() {
        return true;
    }

    @Override
    public void disconnect() throws DebugException {
        // tell backend to stop debugging
        disconnected = true;
    }

    @Override
    public boolean isDisconnected() {
        return disconnected;
    }

    @Override
    public boolean supportsStorageRetrieval() {
        return false;
    }

    @Override
    public IMemoryBlock getMemoryBlock(final long startAddress, final long length)
            throws DebugException {
        return null;
    }

    public IBackend getBackend() {
        return backend;
    }

    public boolean isShowErlideProcesses() {
        return showErlideProcesses;
    }

    public void setShowErlideProcesses(final boolean showErlideProcesses) {
        this.showErlideProcesses = showErlideProcesses;
    }

    public boolean isShowSystemProcesses() {
        return showSystemProcesses;
    }

    public void setShowSystemProcesses(final boolean showSystemProcesses) {
        this.showSystemProcesses = showSystemProcesses;
    }

    public ErlangProcess getOrCreateErlangProcess(final OtpErlangPid pid) {
        ErlangProcess erlangProcess = getErlangProcess(pid);
        if (erlangProcess == null) {
            erlangProcess = createErlangProcess(pid);
        }
        return erlangProcess;
    }

    public Set<String> getInterpretedModules() {
        return interpretedModules;
    }

    private ErlangProcess createErlangProcess(final OtpErlangPid pid) {
        final String theNodeName = pid.node();
        final IDebugTarget[] targets = getLaunch().getDebugTargets();
        for (final IDebugTarget debugTarget : targets) {
            try {
                if (debugTarget.getName().equals(theNodeName)) {
                    if (debugTarget instanceof IErlangDebugNode) {
                        final IErlangDebugNode edn = (IErlangDebugNode) debugTarget;
                        final ErlangProcess p = new ErlangProcess(debugTarget,
                                getBackend(), pid);
                        edn.addErlangProcess(p);
                        allProcesses.add(p);
                        return p;
                    }
                }
            } catch (final DebugException e) {
                ErlLogger.error(e);
            }
        }
        final ErlangProcess p = new ErlangProcess(this, getBackend(), pid);
        addErlangProcess(p);
        allProcesses.add(p);
        return p;
    }

    public ErlangProcess getErlangProcess(final OtpErlangPid pid) {
        for (int i = 0; i < allProcesses.size(); ++i) {
            final ErlangProcess p = allProcesses.get(i);
            if (p.getPid().equals(pid)) {
                return p;
            }
        }
        return null;
    }

    @SuppressWarnings("unused")
    private void removeErlangProcess(final OtpErlangPid pid) {
        final ErlangProcess p = getErlangProcess(pid);
        if (p != null) {
            allProcesses.remove(p);
            removeErlangProcess(p);
            p.fireTerminateEvent();
        }
    }

    public void sendStarted() {
        ErlideDebug.sendStarted(backend, debuggerDaemon.getMBox());
    }

    public OtpErlangPid getMetaFromPid(final OtpErlangPid pid) {
        return metaPids.get(pid);
    }

    public OtpErlangPid getPidFromMeta(final OtpErlangPid metaPid) {
        return pidsFromMeta.get(metaPid);
    }

    public void putMetaPid(final OtpErlangPid metaPid, final OtpErlangPid pid) {
        metaPids.put(pid, metaPid);
        pidsFromMeta.put(metaPid, pid);
    }

    public Collection<IProject> getProjects() {
        return Collections.unmodifiableCollection(projects);
    }

    @Override
    public void addErlangProcess(final ErlangProcess p) {
        localProcesses.add(p);
    }

    @Override
    public void removeErlangProcess(final ErlangProcess p) {
        localProcesses.remove(p);
    }

    @Override
    public ErlangDebugTarget getErlangDebugTarget() {
        return this;
    }

    public Collection<OtpErlangPid> getAllMetaPids() {
        return metaPids.values();
    }

    public OtpErlangPid getEventMBox() {
        return debuggerDaemon.getMBox();
    }

    public void interpretModules(final Collection<String> modules,
            final boolean distributed) {
        for (final String pm : modules) {
            final String[] pms = pm.split(":");
            final IProject project = ResourcesPlugin.getWorkspace().getRoot()
                    .getProject(pms[0]);
            final String moduleName = pms[1].replace(".erl", "");
            interpret(project, moduleName, distributed, true);
        }
    }

    public void interpret(final IProject project, final String moduleName,
            final boolean distributed, final boolean interpret) {
        ErlLogger.debug((interpret ? "" : "de") + "interpret " + moduleName);
        final OtpErlangList options = getProjectDirs(project);
        ErlideDebug.interpret(backend.getOtpRpc(), moduleName, options, distributed,
                interpret);
    }

    private OtpErlangList getProjectDirs(final IProject project) {
        final IErlModel model = ErlangEngine.getInstance().getModel();
        final ErlangProjectProperties properties = model.findProject(project)
                .getProperties();
        final String ebin = properties.getOutputDir().toPortableString();
        final Collection<IPath> srcs = properties.getSourceDirs();
        try {
            return (OtpErlangList) ErlUtils.format("[{ebin_dir, ~s}, {src_dirs, ~ls}]",
                    ebin, srcs);
        } catch (final TermParserException e) {
            ErlLogger.warn(e);
        } catch (final SignatureException e) {
            ErlLogger.warn(e);
        }
        return new OtpErlangList();
    }

    private void distributeDebuggerCode() {
        final List<String> debuggerModules = getDebuggerModules();

        final List<OtpErlangTuple> modules = new ArrayList<OtpErlangTuple>(
                debuggerModules.size());
        for (final String module : debuggerModules) {
            final OtpErlangBinary b = getDebuggerBeam(module);
            if (b != null) {
                final OtpErlangString filename = new OtpErlangString(module + ".erl");
                final OtpErlangTuple t = OtpErlang.mkTuple(new OtpErlangAtom(module),
                        filename, b);
                modules.add(t);
            } else {
                ErlLogger.warn("Could not find debugger module %s", module);
            }
        }
        ErlideDebug.distributeDebuggerCode(backend.getOtpRpc(), modules);
    }

    private void unloadDebuggerCode() {
        final List<String> debuggerModules = getDebuggerModules();
        ErlideDebug.unloadDebuggerCode(backend.getOtpRpc(), debuggerModules);
    }

    /**
     * Get a named beam-file as a binary from the debugger plug-in bundle
     */
    private OtpErlangBinary getDebuggerBeam(final String module) {
        final String beamname = module + ".beam";
        final Bundle bundle = Platform.getBundle("org.erlide.kernel.debugger");

        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] els = reg.getConfigurationElementsFor(
                BackendActivator.PLUGIN_ID, "codepath");

        // TODO: this code assumes that the debugged debugTarget and the
        // erlide-plugin uses the same Erlang version, how can we escape this?

        for (final IConfigurationElement el : els) {
            final IContributor c = el.getContributor();
            final String name = c.getName();
            if (name.equals(bundle.getSymbolicName())) {
                final String dirPath = el.getAttribute("path");
                final Enumeration<?> e = bundle.getEntryPaths(dirPath);
                if (e == null) {
                    ErlLogger.error("* !!! error loading plugin "
                            + bundle.getSymbolicName());
                    return null;
                }
                while (e.hasMoreElements()) {
                    final String s = (String) e.nextElement();
                    final Path path = new Path(s);
                    if (path.lastSegment().equals(beamname)) {
                        return getBeamFromBundlePath(bundle, s, path);
                    }
                }
            }
        }
        return null;
    }

    private OtpErlangBinary getBeamFromBundlePath(@NonNull final Bundle bundle,
            final String s, final Path path) {
        final String m = path.removeFileExtension().lastSegment();
        try {
            return BeamUtil.getBeamBinary(m, bundle.getEntry(s));
        } catch (final Exception ex) {
            ErlLogger.warn(ex);
            return null;
        }
    }

    private void addNodesAsDebugTargets(final ILaunch aLaunch) {
        final OtpErlangList nodes = ErlideDebug.nodes(backend.getOtpRpc());
        if (nodes != null) {
            for (int i = 1, n = nodes.arity(); i < n; ++i) {
                final OtpErlangAtom a = (OtpErlangAtom) nodes.elementAt(i);
                final IDebugTarget edn = new ErlangDebugNode(this, a.atomValue());
                aLaunch.addDebugTarget(edn);
            }
        }
    }

    private List<String> getDebuggerModules() {
        final Bundle debugger = Platform.getBundle("org.erlide.kernel.debugger");
        final List<String> dbg_modules = getModulesFromBundle(debugger);

        final String ver = backend.getRuntime().getVersion().asMajor().toString()
                .toLowerCase();
        final Bundle debugger_otp = Platform.getBundle("org.erlide.kernel.debugger.otp."
                + ver);
        final List<String> dbg_otp_modules = getModulesFromBundle(debugger_otp);

        dbg_modules.addAll(dbg_otp_modules);
        return dbg_modules;
    }

    private List<String> getModulesFromBundle(final Bundle bundle) {
        final List<String> modules = Lists.newArrayList();
        @SuppressWarnings("rawtypes")
        final Enumeration beams = bundle.findEntries("/ebin", "*.beam", false);
        while (beams.hasMoreElements()) {
            final URL beam = (URL) beams.nextElement();
            modules.add(new Path(beam.getPath()).removeFileExtension().lastSegment());
        }
        return modules;
    }

    @Override
    public void dispose() {
        if (disposed) {
            return;
        }
        disposed = true;
        unloadDebuggerCode();
    }

}

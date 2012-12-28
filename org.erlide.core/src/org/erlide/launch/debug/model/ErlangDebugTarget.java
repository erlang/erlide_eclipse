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
package org.erlide.launch.debug.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IMemoryBlock;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IThread;
import org.erlide.backend.IBackend;
import org.erlide.launch.debug.DebuggerEventDaemon;
import org.erlide.launch.debug.ErlangLineBreakpoint;
import org.erlide.launch.debug.ErlideDebug;
import org.erlide.launch.debug.IErlangDebugNode;
import org.erlide.utils.ErlLogger;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlangDebugTarget extends ErlangDebugElement implements
        IDebugTarget, IErlangDebugNode {

    private static final OtpErlangAtom PARENT_ATOM = new OtpErlangAtom("parent");

    public static final IThread[] NO_PROCS = new IThread[] {};

    public static final int INTERPRETED_MODULES_CHANGED = 0;
    public static final int TRACE_CHANGED = 1;

    private final List<ErlangProcess> fAllProcesses;
    private final List<ErlangProcess> fLocalProcesses;
    final IBackend fBackend;
    private final ILaunch fLaunch;
    private boolean fDisconnected = false;
    // private final DebuggerListener fDbgListener;
    // private final DebuggerEventListener fDebuggerEventListener;
    private boolean fTerminated;
    private boolean fShowSystemProcesses = false;
    private boolean fShowErlideProcesses = false;
    private final Set<String> interpretedModules;
    private final Collection<IProject> projects;

    private final Map<OtpErlangPid, OtpErlangPid> metaPids = new TreeMap<OtpErlangPid, OtpErlangPid>();
    private final Map<OtpErlangPid, OtpErlangPid> pidsFromMeta = new TreeMap<OtpErlangPid, OtpErlangPid>();

    private final String fNodeName;

    private final DebuggerEventDaemon debuggerDaemon;

    // private final WaitingForDebuggerListener waiter;

    public ErlangDebugTarget(final ILaunch launch, final IBackend b,
            final Collection<IProject> projects, final int debugFlags)
            throws DebugException {
        super(null);
        fBackend = b;
        fNodeName = b.getFullNodeName();
        fLaunch = launch;
        fTerminated = false;
        this.projects = projects;
        fAllProcesses = new ArrayList<ErlangProcess>();
        fLocalProcesses = new ArrayList<ErlangProcess>();
        interpretedModules = new HashSet<String>();

        debuggerDaemon = new DebuggerEventDaemon(b, this);
        debuggerDaemon.start();
        ErlLogger.debug("debug daemon " + debuggerDaemon.getMBox());

        final OtpErlangPid pid = ErlideDebug.startDebug(b, debugFlags);
        ErlLogger.debug("debug started " + pid);
        fBackend.send(pid,
                OtpErlang.mkTuple(PARENT_ATOM, debuggerDaemon.getMBox()));

        DebugPlugin.getDefault().getBreakpointManager()
                .addBreakpointListener(this);
    }

    @Override
    public ILaunch getLaunch() {
        return fLaunch;
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
        return fLocalProcesses.toArray(new IThread[fLocalProcesses.size()]);
    }

    @Override
    public boolean hasThreads() throws DebugException {
        return !isTerminated();
    }

    @Override
    public String getName() throws DebugException {
        return fNodeName;
    }

    @Override
    public boolean supportsBreakpoint(final IBreakpoint breakpoint) {
        // TODO we should ask the Erlang debugger too...
        if (!isTerminated()
                && breakpoint.getModelIdentifier().equals(getModelIdentifier())) {
            final IProject bpProject = breakpoint.getMarker().getResource()
                    .getProject();
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
        return fTerminated;
    }

    @Override
    public void terminate() throws DebugException {
        if (fTerminated) {
            return;
        }

        fBackend.send("erlide_dbg_mon", new OtpErlangAtom("stop"));
        final DebugPlugin dbgPlugin = DebugPlugin.getDefault();
        if (dbgPlugin != null) {
            dbgPlugin.getBreakpointManager().removeBreakpointListener(this);
        }
        fBackend.stop();
        fTerminated = true;

        final ILaunch launch = getLaunch();
        if (launch != null) {
            launch.terminate();
        }
        fBackend.dispose();
        final IProcess process = getProcess();
        if (process != null) {
            process.terminate();
        }
        if (debuggerDaemon != null) {
            debuggerDaemon.stop();
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
        final IBreakpoint[] breakpoints = DebugPlugin.getDefault()
                .getBreakpointManager().getBreakpoints(getModelIdentifier());
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
                        && DebugPlugin.getDefault().getBreakpointManager()
                                .isEnabled() || !breakpoint.isRegistered()) {
                    final ErlangLineBreakpoint erlangLineBreakpoint = (ErlangLineBreakpoint) breakpoint;
                    erlangLineBreakpoint.install(this);
                }
            } catch (final CoreException e) {
                ErlLogger.error(e);
            }
        }

    }

    @Override
    public void breakpointRemoved(final IBreakpoint breakpoint,
            final IMarkerDelta delta) {
        try {
            ErlLogger.debug("breakpointRemoved "
                    + breakpoint.getMarker().toString()
                    + breakpoint.getMarker().getAttribute(IMarker.LINE_NUMBER));
        } catch (final CoreException e) {
        }
        if (supportsBreakpoint(breakpoint)) {
            final ErlangLineBreakpoint erlangLineBreakpoint = (ErlangLineBreakpoint) breakpoint;
            erlangLineBreakpoint.remove(this);
        }
    }

    @Override
    public void breakpointChanged(final IBreakpoint breakpoint,
            final IMarkerDelta delta) {
        if (supportsBreakpoint(breakpoint)) {
            try {
                if (breakpoint.isEnabled()
                        && DebugPlugin.getDefault().getBreakpointManager()
                                .isEnabled()) {
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
        fDisconnected = true;
    }

    @Override
    public boolean isDisconnected() {
        return fDisconnected;
    }

    @Override
    public boolean supportsStorageRetrieval() {
        return false;
    }

    @Override
    public IMemoryBlock getMemoryBlock(final long startAddress,
            final long length) throws DebugException {
        return null;
    }

    public IBackend getBackend() {
        return fBackend;
    }

    public boolean isShowErlideProcesses() {
        return fShowErlideProcesses;
    }

    public void setShowErlideProcesses(final boolean showErlideProcesses) {
        fShowErlideProcesses = showErlideProcesses;
    }

    public boolean isShowSystemProcesses() {
        return fShowSystemProcesses;
    }

    public void setShowSystemProcesses(final boolean showSystemProcesses) {
        fShowSystemProcesses = showSystemProcesses;
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
        final String nodeName = pid.node();
        final IDebugTarget[] targets = getLaunch().getDebugTargets();
        for (final IDebugTarget debugTarget : targets) {
            try {
                if (debugTarget.getName().equals(nodeName)) {
                    if (debugTarget instanceof IErlangDebugNode) {
                        final IErlangDebugNode edn = (IErlangDebugNode) debugTarget;
                        final ErlangProcess p = new ErlangProcess(debugTarget,
                                getBackend(), pid);
                        edn.addErlangProcess(p);
                        fAllProcesses.add(p);
                        return p;
                    }
                }
            } catch (final DebugException e) {
                e.printStackTrace();
            }
        }
        final ErlangProcess p = new ErlangProcess(this, getBackend(), pid);
        addErlangProcess(p);
        fAllProcesses.add(p);
        return p;
    }

    public ErlangProcess getErlangProcess(final OtpErlangPid pid) {
        for (int i = 0; i < fAllProcesses.size(); ++i) {
            final ErlangProcess p = fAllProcesses.get(i);
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
            fAllProcesses.remove(p);
            removeErlangProcess(p);
            p.fireTerminateEvent();
        }
    }

    public void sendStarted() {
        ErlideDebug.sendStarted(fBackend, debuggerDaemon.getMBox());
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
        fLocalProcesses.add(p);
    }

    @Override
    public void removeErlangProcess(final ErlangProcess p) {
        fLocalProcesses.remove(p);
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
}

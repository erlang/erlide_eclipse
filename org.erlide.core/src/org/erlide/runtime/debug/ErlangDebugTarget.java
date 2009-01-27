/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.debug;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IMemoryBlock;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IThread;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.ErlRpcMessageListener;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideDebug;

public class ErlangDebugTarget extends ErlangDebugElement implements
		IDebugTarget, ErlRpcMessageListener {

	private static final IThread[] NO_PROCS = new IThread[] {};

	public static final int INTERPRETED_MODULES_CHANGED = 0;

	private final List<ErlangProcess> fProcesses;
	final Backend fBackend;
	private final ILaunch fLaunch;
	private boolean fDisconnected = false;
	// private final DebuggerListener fDbgListener;
	// private final DebuggerEventListener fDebuggerEventListener;
	private boolean fTerminated;
	private boolean fShowSystemProcesses = false;
	private boolean fShowErlideProcesses = false;
	private final Set<String> interpretedModules;
	private final Collection<IProject> projects;

	private final Map<OtpErlangPid, OtpErlangPid> metaPids = new HashMap<OtpErlangPid, OtpErlangPid>();
	private final Map<OtpErlangPid, OtpErlangPid> pidsFromMeta = new HashMap<OtpErlangPid, OtpErlangPid>();

	// private final WaitingForDebuggerListener waiter;

	public ErlangDebugTarget(final ILaunch launch, final Backend b,
			final Collection<IProject> projects, final int debugFlags) {
		super(null);
		fBackend = b;
		fLaunch = launch;
		fTerminated = false;
		this.projects = projects;
		fProcesses = new ArrayList<ErlangProcess>();
		interpretedModules = new HashSet<String>();

		b.addErlRpcMessageListener(this);

		final OtpErlangPid pid = ErlideDebug.startDebug(b, debugFlags);
		ErlLogger.debug("debug started " + pid);
		fBackend.send(pid, new OtpErlangTuple(new OtpErlangAtom("parent"), b
				.getEventPid()));

		DebugPlugin.getDefault().getBreakpointManager().addBreakpointListener(
				this);
	}

	@Override
	public ILaunch getLaunch() {
		return fLaunch;
	}

	@Override
	public IDebugTarget getDebugTarget() {
		return this;
	}

	public IProcess getProcess() {
		return null;
	}

	public IThread[] getThreads() throws DebugException {
		if (isTerminated()) {
			return NO_PROCS;
		}
		return fProcesses.toArray(new IThread[fProcesses.size()]);
	}

	public boolean hasThreads() throws DebugException {
		return !isTerminated();
	}

	public String getName() throws DebugException {
		return fBackend.getInfo().getName();
	}

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

	public boolean canTerminate() {
		return true;
	}

	public boolean isTerminated() {
		return fTerminated;
	}

	public void terminate() {
		if (fTerminated) {
			return;
		}

		fTerminated = true;
		fBackend.send("erlide_dbg_mon", new OtpErlangAtom("stop"));

		DebugPlugin.getDefault().getBreakpointManager()
				.removeBreakpointListener(this);

		fireTerminateEvent();
	}

	/**
	 * Notification we have connected to the VM and it has started. Resume the
	 * VM.
	 */
	protected void started() {
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
	private void installDeferredBreakpoints() {
		final IBreakpoint[] breakpoints = DebugPlugin.getDefault()
				.getBreakpointManager().getBreakpoints(getModelIdentifier());
		for (int i = 0; i < breakpoints.length; i++) {
			breakpointAdded(breakpoints[i]);
		}
	}

	public boolean canResume() {
		return false;
	}

	public boolean canSuspend() {
		return false;
	}

	public boolean isSuspended() {
		return false;
	}

	public void resume() throws DebugException {
	}

	public void suspend() throws DebugException {
	}

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
				ErlLogger.warn(e);
			}
		}

	}

	public void breakpointRemoved(final IBreakpoint breakpoint,
			final IMarkerDelta delta) {
		if (supportsBreakpoint(breakpoint)) {
			final ErlangLineBreakpoint erlangLineBreakpoint = (ErlangLineBreakpoint) breakpoint;
			erlangLineBreakpoint.remove(this);
		}
	}

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

	public boolean canDisconnect() {
		return true;
	}

	public void disconnect() throws DebugException {
		// tell backend to stop debugging
		fDisconnected = true;
	}

	public boolean isDisconnected() {
		return fDisconnected;
	}

	public boolean supportsStorageRetrieval() {
		return false;
	}

	public IMemoryBlock getMemoryBlock(final long startAddress,
			final long length) throws DebugException {
		return null;
	}

	public Backend getBackend() {
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

	private void handleMetaEvent(final OtpErlangPid metaPid,
			final OtpErlangTuple metaEvent) {
		ErlLogger.debug("handleMetaEvent " + metaEvent);
		final OtpErlangAtom a = (OtpErlangAtom) metaEvent.elementAt(0);
		final String event = a.atomValue();
		if (event.equals("break_at")) {
			final OtpErlangAtom mod = (OtpErlangAtom) metaEvent.elementAt(1);
			final OtpErlangLong lineL = (OtpErlangLong) metaEvent.elementAt(2);
			int line = 0;
			try {
				line = lineL.intValue();
			} catch (final OtpErlangRangeException e1) {
				ErlLogger.warn(e1);
			}
			final OtpErlangPid pid = getPidFromMeta(metaPid);
			ErlangProcess erlangProcess = getErlangProcess(pid);
			if (erlangProcess == null) {
				erlangProcess = addErlangProcess(pid);
			}
			erlangProcess.breakAt(mod.atomValue(), line);
			if (erlangProcess.isStepping()) {
				erlangProcess.fireSuspendEvent(DebugEvent.STEP_END);
			} else {
				erlangProcess.fireSuspendEvent(DebugEvent.BREAKPOINT);
			}
			erlangProcess.setNotStepping();
		}
	}

	private void handleIntEvent(final OtpErlangTuple intEvent) {
		final OtpErlangAtom a = (OtpErlangAtom) intEvent.elementAt(0);
		final String event = a.atomValue();
		if (event.equals("new_break")) {
			// TODO should we do anything here?
		} else if (event.equals("new_status")) {
			final OtpErlangPid pid = (OtpErlangPid) intEvent.elementAt(1);
			ErlangProcess erlangProcess = getErlangProcess(pid);
			if (erlangProcess == null) {
				erlangProcess = addErlangProcess(pid);
			}
			final OtpErlangAtom sa = (OtpErlangAtom) intEvent.elementAt(2);
			final String status = sa.atomValue();
			if (status.equals("break")) {
				erlangProcess.setStatus(status);
				if (!erlangProcess.isStepping()) {
					fireSuspendEvent(DebugEvent.BREAKPOINT);
				}
			} else if (status.equals("exit")) {
				removeErlangProcess(pid);
				erlangProcess.fireTerminateEvent();
			} else if (status.equals("running")) {
				erlangProcess.setStatus(status);
				if (erlangProcess.isStepping()) {
					fireResumeEvent(DebugEvent.STEP_OVER);
				} else {
					fireResumeEvent(DebugEvent.RESUME);
				}
			} else {
				erlangProcess.setStatus(status);
				fireChangeEvent(DebugEvent.STATE | DebugEvent.CHANGE);
			}
		} else if (event.equals("new_process")) {
			final OtpErlangTuple t = (OtpErlangTuple) intEvent.elementAt(1);
			final OtpErlangPid pid = (OtpErlangPid) t.elementAt(0);
			final ErlangProcess erlangProcess = addErlangProcess(pid);
			final OtpErlangAtom statusA = (OtpErlangAtom) t.elementAt(2);
			final String status = statusA.atomValue();
			erlangProcess.setStatus(status);
			erlangProcess.fireCreationEvent();
		} else if (event.equals("interpret")) {
			final OtpErlangAtom m = (OtpErlangAtom) intEvent.elementAt(1);
			interpretedModules.add(m.atomValue());
			fireEvent(new DebugEvent(this, DebugEvent.MODEL_SPECIFIC,
					INTERPRETED_MODULES_CHANGED));
		} else if (event.equals("no_interpret")) {
			final OtpErlangAtom m = (OtpErlangAtom) intEvent.elementAt(1);
			interpretedModules.remove(m.atomValue());
			fireEvent(new DebugEvent(this, DebugEvent.MODEL_SPECIFIC,
					INTERPRETED_MODULES_CHANGED));
		}
	}

	public Set<String> getInterpretedModules() {
		return interpretedModules;
	}

	private ErlangProcess addErlangProcess(final OtpErlangPid pid) {
		final ErlangProcess p = new ErlangProcess(this, pid);
		fProcesses.add(p);
		return p;
	}

	private ErlangProcess getErlangProcess(final OtpErlangPid pid) {
		for (int i = 0; i < fProcesses.size(); ++i) {
			final ErlangProcess p = fProcesses.get(i);
			if (p.getPid().equals(pid)) {
				return p;
			}
		}
		return null;
	}

	private void removeErlangProcess(final OtpErlangPid pid) {
		final ErlangProcess p = getErlangProcess(pid);
		if (p != null) {
			fProcesses.remove(p);
			p.fireTerminateEvent();

		}
	}

	// private void setErlangProcessStatus(final OtpErlangPid pid,
	// final String status) {
	// final ErlangProcess p = getErlangProcess(pid);
	// if (p != null) {
	// p.setStatus(status);
	// p.fireChangeEvent(DebugEvent.STATE);
	// }
	// }

	// private void setErlangProcessBreakAt(final OtpErlangPid pid,
	// final String mod, final int line) {
	// final ErlangProcess p = getErlangProcess(pid);
	// if (p != null) {
	// p.breakAt(mod, line);
	// }
	// }

	public void sendStarted() {
		ErlideDebug.sendStarted(fBackend, fBackend.getEventPid());
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

	public boolean handleMsg(final OtpErlangObject msg) {
		ErlLogger.debug("### got msg: " + msg);
		if (msg == null) {
			return false;
		}
		// TODO Fler events från erlide_dbg_mon...
		final OtpErlangTuple t = (OtpErlangTuple) msg;
		final OtpErlangObject el0 = t.elementAt(0);
		if (el0 instanceof OtpErlangAtom) {
			final OtpErlangAtom a = (OtpErlangAtom) el0;
			final String event = a.atomValue();
			if (event.equals("started")) {
				started();
			} else if (event.equals("terminated")) {
				terminate();
			} else if (event.equals("int")) {
				handleIntEvent((OtpErlangTuple) t.elementAt(1));
			} else if (event.equals("attached")) {
				final OtpErlangPid pid = (OtpErlangPid) t.elementAt(1);
				if (getMetaFromPid(pid) == null) {
					final OtpErlangPid self = fBackend.getEventPid();
					final OtpErlangPid metaPid = ErlideDebug.attached(fBackend,
							pid, self);
					ErlLogger.debug("attached " + pid + "  meta " + metaPid);
					if (metaPid != null) {
						putMetaPid(metaPid, pid);
					}
				}
			}
			return true;
		} else if (el0 instanceof OtpErlangPid) { // meta event
			final OtpErlangPid pid = (OtpErlangPid) el0;
			final OtpErlangObject metaEvent = t.elementAt(1);
			if (metaEvent instanceof OtpErlangTuple) {
				final OtpErlangTuple metaEventTuple = (OtpErlangTuple) metaEvent;
				handleMetaEvent(pid, metaEventTuple);
			}
			return true;
		}
		return false;
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.runtime.backend.ErlRpcMessageListener#handleMsgs(java.util.List)
	 */
	public void handleMsgs(final List<OtpErlangObject> msgs) {
		for (int i = 0; i < msgs.size(); ++i) {
			final OtpErlangObject msg = msgs.get(i);
			if (handleMsg(msg)) {
				msgs.remove(i);
				--i;
			}
		}
	}

	public Collection<IProject> getProjects() {
		return projects;
	}
}

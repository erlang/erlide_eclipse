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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
import org.erlide.runtime.backend.ExecutionBackend;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.IErlRpcMessageListener;
import org.erlide.runtime.backend.internal.AbstractBackend;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideDebug;

public class ErlangDebugTarget extends ErlangDebugElement implements
		IDebugTarget, IErlRpcMessageListener {

	private static final IThread[] NO_PROCS = new IThread[] {};

	List<ErlangProcess> fProcesses;
	final IBackend fBackend;
	private final ILaunch fLaunch;
	private boolean fDisconnected = false;
	// private final DebuggerListener fDbgListener;
	// private final DebuggerEventListener fDebuggerEventListener;
	boolean fTerminated;
	private boolean fShowSystemProcesses = false;
	private boolean fShowErlideProcesses = false;
	IProject[] projects;

	final Map<OtpErlangPid, OtpErlangPid> metaPids = new HashMap<OtpErlangPid, OtpErlangPid>();

	// private final WaitingForDebuggerListener waiter;

	public ErlangDebugTarget(final ILaunch launch, final ExecutionBackend b,
			final IProject[] projects, final int debugFlags) {
		super(null);
		fBackend = b;
		fLaunch = launch;
		fTerminated = false;
		this.projects = projects;
		fProcesses = new ArrayList<ErlangProcess>();

		((AbstractBackend) b).addErlRpcMessageListener(this);

		final OtpErlangPid pid = ErlideDebug.startDebug(b, debugFlags);
		ErlLogger.debug("debug started " + pid);
		fBackend.send(pid, new OtpErlangTuple(new OtpErlangAtom("parent"), b
				.getEventPid()));

		DebugPlugin.getDefault().getBreakpointManager().addBreakpointListener(
				this);
	}

	// public class WaitingForDebuggerListener {
	// public synchronized void doWait() {
	// try {
	// wait(100);
	// } catch (final InterruptedException e) {
	// }
	// }
	//
	// synchronized void doNotify() {
	// notify();
	// }
	// }

	@Override
	public ILaunch getLaunch() {
		return fLaunch;
	}

	@Override
	public IDebugTarget getDebugTarget() {
		return this;
	}

	public IProcess getProcess() {
		// return new DummyProcess(fLaunch);
		return null;
	}

	public IThread[] getThreads() throws DebugException {
		if (isTerminated()) {
			return NO_PROCS;
		}
		return fProcesses.toArray(new IThread[fProcesses.size()]);
	}

	// @SuppressWarnings("unused")
	// private void cmd(final String cmd, final OtpErlangObject args) {
	// final OtpErlangTuple ct = new OtpErlangTuple(new OtpErlangObject[] {
	// new OtpErlangAtom(cmd), args });
	//
	// final OtpErlangTuple msg = new OtpErlangTuple(new OtpErlangObject[] {
	// new OtpErlangAtom("cmd"), ct });
	//
	// fBackend.send("erlide_dbg_mon", msg);
	// }

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

		// fDbgListener.stop();
		// fBackend.removeEventListener("int", fDebuggerEventListener);
		// fBackend.removeEventListener("started", fDebuggerEventListener);
		DebugPlugin.getDefault().getBreakpointManager()
				.removeBreakpointListener(this);
	}

	/**
	 * Notification we have connected to the VM and it has started. Resume the
	 * VM.
	 */
	void started() {
		fireCreationEvent();
		installDeferredBreakpoints();
		try {
			resume();
		} catch (final DebugException e) {
			e.printStackTrace();
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
				e.printStackTrace();
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
				// TODO Auto-generated catch block
				e.printStackTrace();
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

	// private class DebuggerListener {
	// OtpErlangPid fDbgPid;
	//
	// private final ErlEventLoop loop;
	//
	// public DebuggerListener(final String name, final OtpErlangPid dbgPid) {
	// // public DebuggerListener(final String name, final OtpErlangPid
	// // dbgPid,
	// // final WaitingForDebuggerListener waiting) {
	// fDbgPid = dbgPid;
	//
	// // TODO use the new event router job!
	//
	// final IErlEventHandler r = new DebuggerJob();
	// loop = new ErlEventLoop(r);
	// loop.start();
	// // waiting.doNotify();
	// }

	// class DebuggerEventListener implements IBackendEventListener {
	//
	// // private OtpErlangPid self;
	//
	// // public void init() {
	// // self = fBackend.getEventPid();
	// // fBackend.send(fDbgPid, new OtpErlangTuple(new OtpErlangAtom(
	// // "parent"), self));
	// // }
	//
	// /*
	// * (non-Javadoc)
	// *
	// * @see
	// org.erlide.jinterface.rpc.IErlEventHandler#handleEvent(com.ericsson
	// * .otp.erlang.OtpErlangObject)
	// */
	// public void handleEvent(final OtpErlangObject msg) {
	// if (msg == null) {
	// return;
	// }
	// ErlLogger.debug("### got msg: " + msg);
	// // TODO Fler events från erlide_dbg_mon...
	// final OtpErlangTuple t = (OtpErlangTuple) msg;
	// final OtpErlangObject el0 = t.elementAt(0);
	// if (el0 instanceof OtpErlangAtom) {
	// final OtpErlangAtom a = (OtpErlangAtom) el0;
	// final String event = a.atomValue();
	// if (event.equals("started")) {
	// started();
	// } else if (event.equals("terminated")) {
	// terminate();
	// } else if (event.equals("int")) {
	// handleIntEvent((OtpErlangTuple) t.elementAt(1));
	// }
	// } else if (el0 instanceof OtpErlangPid) { // meta event
	// final OtpErlangObject metaEvent = t.elementAt(1);
	// if (metaEvent instanceof OtpErlangTuple) {
	// final OtpErlangTuple metaEventTuple = (OtpErlangTuple) metaEvent;
	// handleMetaEvent(metaEventTuple);
	// }
	// }
	// }

	private void handleMetaEvent(final OtpErlangTuple metaEvent) {
		ErlLogger.debug("handleMetaEvent " + metaEvent);
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
				final OtpErlangTuple modLine = (OtpErlangTuple) intEvent
						.elementAt(3);
				final OtpErlangAtom mod = (OtpErlangAtom) modLine.elementAt(0);
				final OtpErlangLong lineL = (OtpErlangLong) modLine
						.elementAt(1);
				int line = 0;
				try {
					line = lineL.intValue();
				} catch (final OtpErlangRangeException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				erlangProcess.breakAt(mod.atomValue(), line);
				if (erlangProcess.isStepping()) {
					erlangProcess.fireSuspendEvent(DebugEvent.STEP_END);
				} else {
					erlangProcess.fireSuspendEvent(DebugEvent.BREAKPOINT);
				}
				erlangProcess.setNotStepping();
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
			if (metaPids.get(pid) == null) {
				final OtpErlangPid self = fBackend.getEventPid();
				final OtpErlangPid metaPid = ErlideDebug.attached(fBackend,
						pid, self);
				if (metaPid != null) {
					metaPids.put(pid, metaPid);
				}
			}
			erlangProcess.fireCreationEvent();
		}
	}

	// public void eventReceived(final OtpErlangObject event) {
	// handleEvent(event);
	// }
	// }

	// public void stop() {
	// loop.stop();
	// }
	// }

	ErlangProcess addErlangProcess(final OtpErlangPid pid) {
		final ErlangProcess p = new ErlangProcess(this, pid);
		fProcesses.add(p);
		return p;
	}

	ErlangProcess getErlangProcess(final OtpErlangPid pid) {
		for (int i = 0; i < fProcesses.size(); ++i) {
			final ErlangProcess p = fProcesses.get(i);
			if (p.getPid().equals(pid)) {
				return p;
			}
		}
		return null;
	}

	void removeErlangProcess(final OtpErlangPid pid) {
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

	public OtpErlangPid getMetaForPid(final OtpErlangPid pid) {
		return metaPids.get(pid);
	}

	void terminated() {
		fireTerminateEvent();
	}

	public boolean handleMsg(final OtpErlangObject msg) {
		if (msg == null) {
			return false;
		}
		ErlLogger.debug("### got msg: " + msg);
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
			}
			return true;
		} else if (el0 instanceof OtpErlangPid) { // meta event
			final OtpErlangObject metaEvent = t.elementAt(1);
			if (metaEvent instanceof OtpErlangTuple) {
				final OtpErlangTuple metaEventTuple = (OtpErlangTuple) metaEvent;
				handleMetaEvent(metaEventTuple);
			}
			return true;
		}
		return false;
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.runtime.backend.IErlRpcMessageListener#handleMsgs(java.util.List)
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
}

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
import java.util.List;

import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IMemoryBlock;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IThread;
import org.erlide.basiccore.ErlLogger;
import org.erlide.jinterface.rpc.ErlEventLoop;
import org.erlide.jinterface.rpc.IErlEventHandler;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.IErlangLaunchConfigurationAttributes;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideDebug;

public class ErlangDebugTarget extends ErlangDebugElement implements
		IDebugTarget {

	private static final IThread[] NO_PROCS = new IThread[] {};

	private final IBackend fBackend;

	private final ILaunch fLaunch;

	private boolean fDisconnected = false;

	private final DebuggerListener fDbgListener;

	private boolean fTerminated;

	private boolean fShowSystemProcesses = false;

	private boolean fShowErlideProcesses = false;

	public ErlangDebugTarget(final ILaunch launch, final IBackend b) {
		super(null);
		fBackend = b;
		fLaunch = launch;
		fTerminated = false;

		final OtpErlangPid pid = ErlideDebug.startDebug(b, b.getEventPid());

		// start debugger listener job
		fDbgListener = new DebuggerListener("Erlang debugger listener", pid);

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
		// return new DummyProcess(fLaunch);
		return null;
	}

	public IThread[] getThreads() throws DebugException {
		if (isTerminated()) {
			return NO_PROCS;
		}
		OtpErlangList procs = null;
		try {
			procs = ErlideDebug.getProcesses(fBackend, fShowSystemProcesses,
					fShowErlideProcesses);
			fBackend.send("erlide_dbg_mon", new OtpErlangAtom("dumpState"));
		} catch (final Exception e) {
		}
		if (procs == null) {
			return NO_PROCS;
		}
		final int n = procs.arity();
		final List<ErlangProcess> l = new ArrayList<ErlangProcess>(n);
		for (int i = 0; i < n; ++i) {
			l.add(new ErlangProcess(this, (OtpErlangPid) procs.elementAt(i)));
		}
		return l.toArray(new IThread[l.size()]);
	}

	@SuppressWarnings("unused")
	private void cmd(final String cmd, final OtpErlangObject args) {
		final OtpErlangTuple ct = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom(cmd), args });

		final OtpErlangTuple msg = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("cmd"), ct });

		fBackend.send("erlide_dbg_mon", msg);
	}

	public boolean hasThreads() throws DebugException {
		return !isTerminated();
	}

	public String getName() throws DebugException {
		return fBackend.getLabel();
	}

	public boolean supportsBreakpoint(final IBreakpoint breakpoint) {
		if (!isTerminated()
				&& breakpoint.getModelIdentifier().equals(getModelIdentifier())) {
			try {
				final String module = getLaunch()
						.getLaunchConfiguration()
						.getAttribute(
								IErlangLaunchConfigurationAttributes.ATTR_ENODE_MODULE,
								(String) null);
				return module != null && module != ""; // TODO FIXME testa mer!
			} catch (final CoreException e) {
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

		// FIXME this causes an exception... why?
		fDbgListener.stop();

		DebugPlugin.getDefault().getBreakpointManager()
				.removeBreakpointListener(this);
	}

	/**
	 * Notification we have connected to the VM and it has started. Resume the
	 * VM.
	 */
	private void started() {
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
		// TODO Auto-generated method stub
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

	private class DebuggerListener {
		OtpErlangPid fDbgPid;

		private final ErlEventLoop loop;

		public DebuggerListener(final String name, final OtpErlangPid dbgPid) {
			fDbgPid = dbgPid;

			// TODO use the new event router job!

			final IErlEventHandler r = new DebuggerJob();
			loop = new ErlEventLoop(r);
			loop.start();
		}

		private class DebuggerJob implements IErlEventHandler {

			private OtpErlangPid self;

			public void init() {
				self = fBackend.getEventPid();

				fBackend.send(fDbgPid, new OtpErlangTuple(new OtpErlangAtom(
						"parent"), self));
			}

			public void handleEvent(final OtpErlangObject msg) {
				if (msg != null) {
					ErlLogger.debug("### got msg: " + msg);
				}
				// TODO Här ska vi väl ta emot events från antingen erlide_dbg
				// eller erlide_dbg_mon...
				final OtpErlangTuple t = (OtpErlangTuple) msg;
				final OtpErlangAtom a = (OtpErlangAtom) t.elementAt(0);
				final String event = a.atomValue();
				if (event.equals("started")) {
					started();
				} else if (event.equals("terminated")) {
					terminate();
				}
			}

			public boolean isTerminated() {
				return fTerminated;
			}

			public OtpErlangObject receiveEvent(final int timeout)
					throws OtpErlangExit, OtpErlangDecodeException {
				if (timeout < 0) {
					// TODO how to handle stopping, if it's blocked in a
					// receive?
					return fBackend.receiveRpc(60000);
				}
				return fBackend.receiveRpc(timeout);
			}

			public int getTimeout() {
				return 200;
			}

			public boolean exception(final Exception e) {
				e.printStackTrace();
				return false;
			}

		}

		public void stop() {
			loop.stop();
		}

	}

}

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

import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IMemoryBlock;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IThread;
import org.erlide.basiccore.ErlLogger;
import org.erlide.runtime.backend.IBackend;

import com.ericsson.otp.erlang.OtpErlangAtom;
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

	@SuppressWarnings("unused")
	private DebuggerListener fDbgListener;

	private boolean fTerminated;

	private boolean fShowSystemProcesses = false;

	private boolean fShowErlideProcesses = false;

	public ErlangDebugTarget(ILaunch launch, IBackend b, String mod, String func) {
		super(null);
		fBackend = b;
		fLaunch = launch;
		fTerminated = false;

		try {
			final OtpErlangPid pid = ErlideDebug.startDebug(b, mod, func);

			// start debugger listener job
			fDbgListener = new DebuggerListener("Erlang debugger listener",
					this, b, pid);
		} catch (final Exception e) {
		}

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

		final OtpErlangObject[] obj = procs.elements();
		final OtpErlangPid[] ps = new OtpErlangPid[obj.length];
		System.arraycopy(obj, 0, ps, 0, ps.length);

		final IThread[] res = new IThread[ps.length];

		for (int i = 0; i < ps.length; i++) {
			final ErlangProcess t = new ErlangProcess(this, ps[i]);
			res[i] = t;
		}
		return res;
	}

	@SuppressWarnings("unused")
	private void cmd(String cmd, OtpErlangObject args) {
		final OtpErlangObject[] ctl = new OtpErlangObject[2];
		ctl[0] = new OtpErlangAtom(cmd);
		ctl[1] = args;
		final OtpErlangTuple ct = new OtpErlangTuple(ctl);

		final OtpErlangObject[] msgl = new OtpErlangObject[2];
		msgl[0] = new OtpErlangAtom("cmd");
		msgl[1] = ct;
		final OtpErlangTuple msg = new OtpErlangTuple(msgl);

		fBackend.send("erlide_dbg_mon", msg);
	}

	public boolean hasThreads() throws DebugException {
		return !isTerminated();
	}

	public String getName() throws DebugException {
		return fBackend.getLabel();
	}

	public boolean supportsBreakpoint(IBreakpoint breakpoint) {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean canTerminate() {
		return true;
	}

	public boolean isTerminated() {
		return fTerminated;
	}

	public void terminate() throws DebugException {
		if (fTerminated) {
			return;
		}

		fTerminated = true;
		fBackend.send("erlide_dbg_mon", new OtpErlangAtom("stop"));

		// FIXME this causes an exception... why?
		// fDbgListener.stop();

		DebugPlugin.getDefault().getBreakpointManager()
				.removeBreakpointListener(this);
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

	public void breakpointAdded(IBreakpoint breakpoint) {
		ErlLogger.debug("Breakpoint added: " + breakpoint);
		// TODO Auto-generated method stub
	}

	public void breakpointRemoved(IBreakpoint breakpoint, IMarkerDelta delta) {
		// TODO Auto-generated method stub
	}

	public void breakpointChanged(IBreakpoint breakpoint, IMarkerDelta delta) {
		// TODO Auto-generated method stub
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

	public IMemoryBlock getMemoryBlock(long startAddress, long length)
			throws DebugException {
		return null;
	}

	public IBackend getBackend() {
		return fBackend;
	}

	public boolean isShowErlideProcesses() {
		return fShowErlideProcesses;
	}

	public void setShowErlideProcesses(boolean showErlideProcesses) {
		fShowErlideProcesses = showErlideProcesses;
	}

	public boolean isShowSystemProcesses() {
		return fShowSystemProcesses;
	}

	public void setShowSystemProcesses(boolean showSystemProcesses) {
		fShowSystemProcesses = showSystemProcesses;
	}

}

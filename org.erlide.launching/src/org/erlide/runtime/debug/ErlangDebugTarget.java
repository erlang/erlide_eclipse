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
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.IErlangLaunchConfigurationAttributes;

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

	private DebuggerListener fDbgListener;

	private boolean fTerminated;

	private boolean fShowSystemProcesses = false;

	private boolean fShowErlideProcesses = false;

	public ErlangDebugTarget(final ILaunch launch, final IBackend b,
			final String mod, final String func) {
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

	public void terminate() throws DebugException {
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
		ErlLogger.debug("Breakpoint added: " + breakpoint);
		// TODO Auto-generated method stub
	}

	public void breakpointRemoved(final IBreakpoint breakpoint,
			final IMarkerDelta delta) {
		// TODO Auto-generated method stub
	}

	public void breakpointChanged(final IBreakpoint breakpoint,
			final IMarkerDelta delta) {
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
}

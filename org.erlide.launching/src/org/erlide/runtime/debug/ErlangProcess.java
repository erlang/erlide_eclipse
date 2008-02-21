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

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;
import org.erlide.basiccore.ErlLogger;
import org.erlide.runtime.backend.IBackend;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideDebug;

public class ErlangProcess extends ErlangDebugElement implements IThread {
	public static final String STATUS_WAITING = "waiting";

	public static final String STATUS_RUNNING = "running";

	public static final String STATUS_RUNNABLE = "runnable";

	public static final String STATUS_SUSPENDED = "suspended";

	public static final String STATUS_TERMINATED = "terminated";

	private final OtpErlangPid fPid;

	private final ErlangDebugTarget fTarget;

	private final IBackend fBackend;

	public ErlangProcess(ErlangDebugTarget target, OtpErlangPid pid) {
		super(target);
		fPid = pid;
		fTarget = target;
		fBackend = target.getBackend();
	}

	public String getRegisteredName() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"registered_name");
		if (res != null) {
			return res.toString();
		}
		return null;
	}

	public OtpErlangTuple getCurrentFunction() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"current_function");
		return (OtpErlangTuple) res;
	}

	public long getReductions() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"reductions");
		if (res != null) {
			try {
				return ((OtpErlangLong) res).longValue();
			} catch (OtpErlangRangeException e) {
			}
		}
		return -1;
	}

	public OtpErlangObject getDictionary() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"dictionary");
		return res;
	}

	public OtpErlangObject getErrorHandler() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"error_handler");
		return res;
	}

	public OtpErlangObject getGroupLeader() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"group_leader");
		return res;
	}

	public OtpErlangObject getHeapSize() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"heap_size");
		return res;
	}

	public OtpErlangObject getInitialCall() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"initial_call");
		return res;
	}

	public OtpErlangObject getLinks() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"links");
		return res;
	}

	public OtpErlangObject getMessageQueueLen() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"message_queue_len");
		return res;
	}

	public OtpErlangObject getMessages() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"messages");
		return res;
	}

	public OtpErlangObject getErlPriority() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"priority");
		return res;
	}

	public OtpErlangObject getStackSize() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"stack_size");
		return res;
	}

	public String getStatus() {
		OtpErlangAtom res = (OtpErlangAtom) ErlideDebug.getProcessInfo(
				fBackend, fPid, "status");
		if (res != null) {
			return res.atomValue();
		}
		return STATUS_TERMINATED;
	}

	public boolean getTrapExit() {
		OtpErlangAtom res = (OtpErlangAtom) ErlideDebug.getProcessInfo(
				fBackend, fPid, "trap_exit");
		return "true".equals(res.atomValue());
	}

	public OtpErlangObject getBacktrace() {
		OtpErlangBinary res = (OtpErlangBinary) ErlideDebug.getProcessInfo(
				fBackend, fPid, "backtrace");
		// byte[] r = res.binaryValue();
		return res;
	}

	public OtpErlangObject getLastCalls() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"last_calls");
		return res;
	}

	public OtpErlangObject getMemory() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"memory");
		return res;
	}

	public OtpErlangObject getMonitoredBy() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"monitored_by");
		return res;
	}

	public OtpErlangObject getMonitors() {
		OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"monitors");
		return res;
	}

	public IStackFrame[] getStackFrames() throws DebugException {
		ErlLogger.debug("** get stackframes for " + fPid);

		if (!isSuspended() && !isStepping()) {
			return new IStackFrame[] { new ErlangStackFrame("" + isSuspended()
					+ "." + isStepping(), this, fTarget) };
		}

		return new IStackFrame[] {};

	}

	public boolean hasStackFrames() throws DebugException {
		return isDebugged();
	}

	private boolean isDebugged() {
		return true;
	}

	public int getPriority() throws DebugException {
		// TODO Auto-generated method stub
		return 0;
	}

	public IStackFrame getTopStackFrame() throws DebugException {
		// TODO Auto-generated method stub
		ErlLogger.debug("get top stackframe");
		return null;
	}

	public String getName() throws DebugException {
		return toLocalPid(fPid);
	}

	public static String toLocalPid(OtpErlangPid pid) {
		// TODO check it!
		final int a1 = pid.id();
		final int a2 = pid.serial();
		return "<0." + a1 + "." + a2 + ">";
	}

	public IBreakpoint[] getBreakpoints() {
		return new IBreakpoint[] {};
	}

	public boolean canResume() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean canSuspend() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isSuspended() {
		return getStatus() == STATUS_SUSPENDED;
	}

	public void resume() throws DebugException {
		// TODO Auto-generated method stub

	}

	public void suspend() throws DebugException {
		// TODO Auto-generated method stub

	}

	public boolean canStepInto() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean canStepOver() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean canStepReturn() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isStepping() {
		// TODO Auto-generated method stub
		return false;
	}

	public void stepInto() throws DebugException {
		// TODO Auto-generated method stub

	}

	public void stepOver() throws DebugException {
		// TODO Auto-generated method stub

	}

	public void stepReturn() throws DebugException {
		// TODO Auto-generated method stub

	}

	public boolean canTerminate() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isTerminated() {
		return getStatus() == STATUS_TERMINATED;
	}

	public void terminate() throws DebugException {
	}

	public boolean isSystemProcess() {
		return ErlideDebug.isSystemProcess_(fBackend, fPid);
	}

	public boolean isErlideProcess() {
		return ErlideDebug.isErlideProcess(fBackend, fPid);
	}

}

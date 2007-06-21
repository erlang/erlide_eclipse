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
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangProcess extends ErlangDebugElement implements IThread {
	public static final String STATUS_WAITING = "waiting";

	public static final String STATUS_RUNNING = "running";

	public static final String STATUS_RUNNABLE = "runnable";

	public static final String STATUS_SUSPENDED = "suspended";

	public static final String STATUS_TERMINATED = "terminated";

	private OtpErlangPid fPid;

	private ErlangDebugTarget fTarget;

	private IBackend fBackend;

	public ErlangProcess(ErlangDebugTarget target, OtpErlangPid pid) {
		super(target);
		fPid = pid;
		fTarget = target;
		fBackend = target.getBackend();
	}

	public String getRegisteredName() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "registered_name");
		if (res != null) {
			return res.toString();
		}
		return null;
	}

	public OtpErlangTuple getCurrentFunction() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "current_function");
		return (OtpErlangTuple) res;
	}

	public long getReductions() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "reductions");
		if (res != null) {
			try {
				return ((OtpErlangLong) res).longValue();
			} catch (OtpErlangRangeException e) {
			}
		}
		return -1;
	}

	public OtpErlangObject getDictionary() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "dictionary");
		return res;
	}

	public OtpErlangObject getErrorHandler() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "error_handler");
		return res;
	}

	public OtpErlangObject getGroupLeader() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "group_leader");
		return res;
	}

	public OtpErlangObject getHeapSize() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "heap_size");
		return res;
	}

	public OtpErlangObject getInitialCall() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "initial_call");
		return res;
	}

	public OtpErlangObject getLinks() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "links");
		return res;
	}

	public OtpErlangObject getMessageQueueLen() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid,
				"message_queue_len");
		return res;
	}

	public OtpErlangObject getMessages() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "messages");
		return res;
	}

	public OtpErlangObject getErlPriority() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "priority");
		return res;
	}

	public OtpErlangObject getStackSize() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "stack_size");
		return res;
	}

	public String getStatus() {
		OtpErlangAtom res = (OtpErlangAtom) getProcessInfo(fBackend, fPid,
				"status");
		if (res != null) {
			return res.atomValue();
		}
		return STATUS_TERMINATED;
	}

	public boolean getTrapExit() {
		OtpErlangAtom res = (OtpErlangAtom) getProcessInfo(fBackend, fPid,
				"trap_exit");
		return "true".equals(res.atomValue());
	}

	public OtpErlangObject getBacktrace() {
		OtpErlangBinary res = (OtpErlangBinary) getProcessInfo(fBackend, fPid,
				"backtrace");
		// byte[] r = res.binaryValue();
		return res;
	}

	public OtpErlangObject getLastCalls() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "last_calls");
		return res;
	}

	public OtpErlangObject getMemory() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "memory");
		return res;
	}

	public OtpErlangObject getMonitoredBy() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "monitored_by");
		return res;
	}

	public OtpErlangObject getMonitors() {
		OtpErlangObject res = getProcessInfo(fBackend, fPid, "monitors");
		return res;
	}

	private static OtpErlangObject getProcessInfo(IBackend b, OtpErlangPid pid,
			String item) {
		OtpErlangObject res;
		try {
			res = BackendUtil.checkRpc(b.rpc("erlang", "process_info", pid,
					new OtpErlangAtom(item)));
			if (res instanceof OtpErlangTuple) {
				return ((OtpErlangTuple) res).elementAt(1);
			} else {
				return null;
			}
		} catch (final BackendException e) {
			return null;
		}

	}

	public IStackFrame[] getStackFrames() throws DebugException {
		ErlLogger.log("** get stackframes for " + fPid);

		if (!isSuspended() && !isStepping()) {
			return new IStackFrame[] { new ErlangStackFrame("" + isSuspended() +
					"." + isStepping(), this, fTarget) };
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
		ErlLogger.log("get top stackframe");
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
		boolean res = false;
		try {
			RpcResult r = fBackend.rpc("pman_process", "is_system_process",
					fPid);
			OtpErlangAtom eres = (OtpErlangAtom) BackendUtil.checkRpc(r);
			res = "true".equals(eres.atomValue());
		} catch (ErlangRpcException e) {
		} catch (BackendException e) {
		}
		return res;
	}

	public boolean isErlideProcess() {
		boolean res = false;
		try {
			RpcResult r = fBackend.rpc("erlide_debug", "is_erlide_process",
					fPid);
			OtpErlangAtom eres = (OtpErlangAtom) BackendUtil.checkRpc(r);
			res = "true".equals(eres.atomValue());
		} catch (ErlangRpcException e) {
		} catch (BackendException e) {
		}
		return res;
	}

}

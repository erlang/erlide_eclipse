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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IBreakpointManager;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.ExecutionBackend;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
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

	public static final String STATUS_UNKNOWN = "unknown";

	public static final String STATUS_BREAK = "break";

	public static final String STATUS_IDLE = "idle";

	private final OtpErlangPid fPid;

	private OtpErlangPid cachedMetaPid = null;

	private final ExecutionBackend fBackend;

	private String fStatus;

	private List<ErlangStackFrame> stackFrames;

	private boolean stepping;

	public ErlangProcess(final ErlangDebugTarget target, final OtpErlangPid pid) {
		super(target);
		fPid = pid;
		fBackend = target.getBackend();
		fStatus = STATUS_UNKNOWN;
		stackFrames = new ArrayList<ErlangStackFrame>();
		stepping = false;
	}

	public String getRegisteredName() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"registered_name");
		if (res != null) {
			return res.toString();
		}
		return null;
	}

	public OtpErlangTuple getCurrentFunction() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"current_function");
		return (OtpErlangTuple) res;
	}

	public OtpErlangPid getMeta() {
		if (cachedMetaPid == null) {
			cachedMetaPid = ((ErlangDebugTarget) getDebugTarget())
					.getMetaFromPid(fPid);
		}
		return cachedMetaPid;
	}

	public long getReductions() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"reductions");
		if (res != null) {
			try {
				return ((OtpErlangLong) res).longValue();
			} catch (final OtpErlangRangeException e) {
			}
		}
		return -1;
	}

	public OtpErlangObject getDictionary() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"dictionary");
		return res;
	}

	public OtpErlangObject getErrorHandler() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"error_handler");
		return res;
	}

	public OtpErlangObject getGroupLeader() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"group_leader");
		return res;
	}

	public OtpErlangObject getHeapSize() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"heap_size");
		return res;
	}

	public OtpErlangObject getInitialCall() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"initial_call");
		return res;
	}

	public OtpErlangObject getLinks() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"links");
		return res;
	}

	public OtpErlangObject getMessageQueueLen() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"message_queue_len");
		return res;
	}

	public OtpErlangObject getMessages() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"messages");
		return res;
	}

	public OtpErlangObject getErlPriority() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"priority");
		return res;
	}

	public OtpErlangObject getStackSize() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"stack_size");
		return res;
	}

	// public String getStatus() {
	// final OtpErlangAtom res = (OtpErlangAtom) ErlideDebug.getProcessInfo(
	// fBackend, fPid, "status");
	// if (res != null) {
	// return res.atomValue();
	// }
	// return STATUS_TERMINATED;
	// }

	public String getStatus() {
		return fStatus;
	}

	public void setStatus(final String status) {
		fStatus = status;
	}

	public boolean getTrapExit() {
		final OtpErlangAtom res = (OtpErlangAtom) ErlideDebug.getProcessInfo(
				fBackend, fPid, "trap_exit");
		return "true".equals(res.atomValue());
	}

	public void breakAt(String module, int line) {
		// final OtpErlangList bindings = ErlideDebug.getBindings(fBackend,
		// getMeta());
		// ErlLogger.debug("bindings " + bindings);
		ErlLogger.debug("breakAt getMeta() " + getMeta());
		final OtpErlangTuple stackAndBindings = ErlideDebug.getAllStackframes(
				fBackend, getMeta());
		final OtpErlangList erlStackFrames = (OtpErlangList) stackAndBindings
				.elementAt(0);
		OtpErlangList bs = (OtpErlangList) stackAndBindings.elementAt(1);
		stackFrames = new ArrayList<ErlangStackFrame>();
		// [{{Mod, Fun, Arity}, {Mod, LineNo}, [Binding...]}...]
		for (final OtpErlangObject o : erlStackFrames.elements()) {
			final OtpErlangTuple t = (OtpErlangTuple) o;
			final OtpErlangTuple ml = (OtpErlangTuple) t.elementAt(1);
			final OtpErlangAtom m = (OtpErlangAtom) ml.elementAt(0);
			final OtpErlangLong l = (OtpErlangLong) ml.elementAt(1);
			final OtpErlangLong n = (OtpErlangLong) t.elementAt(3);
			int stackFrameNo;
			try {
				stackFrameNo = n.intValue();
			} catch (final OtpErlangRangeException e) {
				stackFrameNo = -1;
			}
			stackFrames.add(new ErlangStackFrame(module, this,
					getDebugTarget(), line, bs, stackFrameNo));
			bs = (OtpErlangList) t.elementAt(2);
			module = m.atomValue();
			try {
				line = l.intValue();
			} catch (final OtpErlangRangeException e) {
				line = -1;
			}
		}
		// topFrame = new ErlangStackFrame(module, this, getDebugTarget(), line,
		// bindings);
		// fakeFrame = new ErlangStackFrame(module, this, getDebugTarget(), 12,
		// new OtpErlangList());
	}

	// public OtpErlangObject getBacktrace() {
	// final OtpErlangBinary res = (OtpErlangBinary) ErlideDebug
	// .getProcessInfo(fBackend, fPid, "backtrace");
	// // byte[] r = res.binaryValue();
	// return res;
	// }

	public OtpErlangObject getLastCalls() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"last_calls");
		return res;
	}

	public OtpErlangObject getMemory() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"memory");
		return res;
	}

	public OtpErlangObject getMonitoredBy() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"monitored_by");
		return res;
	}

	public OtpErlangObject getMonitors() {
		final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
				"monitors");
		return res;
	}

	public IStackFrame[] getStackFrames() throws DebugException {
		return stackFrames.toArray(new IStackFrame[stackFrames.size()]);
	}

	public boolean hasStackFrames() throws DebugException {
		return isSuspended();
	}

	public int getPriority() throws DebugException {
		return 0;
	}

	public IStackFrame getTopStackFrame() throws DebugException {
		if (stackFrames.isEmpty()) {
			return null;
		}
		return stackFrames.get(0);
	}

	public String getName() throws DebugException {
		return toLocalPid(fPid);
	}

	public static String toLocalPid(final OtpErlangPid pid) {
		// TODO check it!
		final int a1 = pid.id();
		final int a2 = pid.serial();
		return "<0." + a1 + "." + a2 + ">";
	}

	public IBreakpoint[] getBreakpoints() {
		if (stackFrames.isEmpty()) {
			return null;
		}
		final ErlangStackFrame topFrame = stackFrames.get(0);
		if (topFrame != null) {
			final IBreakpointManager breakpointManager = DebugPlugin
					.getDefault().getBreakpointManager();
			final IBreakpoint[] breakpoints = breakpointManager
					.getBreakpoints();

			for (final IBreakpoint breakpoint : breakpoints) {
				if (breakpoint instanceof ErlangLineBreakpoint) {
					final ErlangLineBreakpoint lineBreakpoint = (ErlangLineBreakpoint) breakpoint;
					try {
						if (lineBreakpoint.getModule().equals(
								topFrame.getModule())
								&& lineBreakpoint.getLineNumber() == topFrame
										.getLineNumber()) {
							return new IBreakpoint[] { lineBreakpoint };
						}
					} catch (final DebugException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (final CoreException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
		}
		return new IBreakpoint[0];
	}

	public boolean canResume() {
		return isSuspended();
	}

	public boolean canSuspend() {
		return !isSuspended();
	}

	public boolean isSuspended() {
		// return getStatus().equals(STATUS_SUSPENDED)
		// || getStatus().equals(STATUS_BREAK)
		// || getStatus().equals(STATUS_IDLE);
		return getStatus().equals(STATUS_SUSPENDED)
				|| getStatus().equals(STATUS_BREAK);
	}

	public void resume() throws DebugException {
		stepping = false;
		ErlideDebug.resume(fBackend, getMeta());
	}

	public void suspend() throws DebugException {
		stepping = false;
		ErlideDebug.suspend(fBackend, getMeta());
	}

	public boolean canStepInto() {
		return isSuspended();
	}

	public boolean canStepOver() {
		return isSuspended();
	}

	public boolean canStepReturn() {
		return isSuspended();
	}

	public boolean isStepping() {
		return stepping;
	}

	public void setNotStepping() {
		stepping = false;
	}

	public void stepInto() throws DebugException {
		stepping = true;
		ErlideDebug.stepInto(fBackend, getMeta());
	}

	public void stepOver() throws DebugException {
		stepping = true;
		ErlideDebug.stepOver(fBackend, getMeta());
	}

	public void stepReturn() throws DebugException {
		stepping = true;
		ErlideDebug.stepReturn(fBackend, getMeta());
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
		return ErlideDebug.isSystemProcess(fBackend, fPid);
	}

	public boolean isErlideProcess() {
		return ErlideDebug.isErlideProcess(fBackend, fPid);
	}

	/**
	 * @return the fPid
	 */
	public OtpErlangPid getPid() {
		return fPid;
	}

}

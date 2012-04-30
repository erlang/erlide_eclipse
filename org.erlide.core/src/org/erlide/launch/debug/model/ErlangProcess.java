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
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IBreakpointManager;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;
import org.erlide.backend.IBackend;
import org.erlide.core.ErlangCore;
import org.erlide.core.model.util.ErlangFunction;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.debug.ErlangLineBreakpoint;
import org.erlide.launch.debug.ErlideDebug;
import org.erlide.utils.ErlangFunctionCall;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
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
    public static final String STATUS_TERMINATED = "exit";
    public static final String STATUS_UNKNOWN = "unknown";
    public static final String STATUS_BREAK = "break";
    public static final String STATUS_IDLE = "idle";

    private final OtpErlangPid fPid;

    private OtpErlangPid fCachedMetaPid = null;

    private final IBackend fBackend;

    private String fStatus;

    private String fExitStatus;

    private List<IStackFrame> stackFrames;

    private boolean stepping;
    private ErlangFunctionCall fInitialCall;
    private boolean fTracing;

    public ErlangProcess(final IDebugTarget target, final IBackend backend,
            final OtpErlangPid pid) {
        super(target);
        fPid = pid;
        fBackend = backend;
        fStatus = STATUS_UNKNOWN;
        fExitStatus = null;
        stackFrames = new ArrayList<IStackFrame>();
        stepping = false;
        fTracing = false;
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
        if (fCachedMetaPid == null) {
            fCachedMetaPid = getErlangDebugTarget().getMetaFromPid(fPid);
        }
        return fCachedMetaPid;
    }

    public long getReductions() {
        final OtpErlangObject res = ErlideDebug.getProcessInfo(fBackend, fPid,
                "reductions");
        if (res != null) {
            return ((OtpErlangLong) res).longValue();
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

    public ErlangFunctionCall getInitialCall() {
        return fInitialCall;
    }

    public void setInitialCall(final ErlangFunctionCall initialCall) {
        fInitialCall = initialCall;
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
    // runtime, fPid, "status");
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
        return Boolean.parseBoolean(res.atomValue());
    }

    public void getStackAndBindings(final String module, final int line) {
        final OtpErlangTuple stackAndBindings = ErlideDebug.getAllStackframes(
                fBackend, getMeta());
        if (stackAndBindings == null) {
            ErlLogger.warn("could not retrieve stack -"
                    + "- are there more than one debug sessions started?");
            return;
        }
        OtpErlangObject savedStackTrace = null;
        // BindingsImpl b = ErlUtils.match("{ST:t, F:l, }", stackAndBindings);
        OtpErlangObject el0 = stackAndBindings.elementAt(0);
        if (el0 instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) el0;
            savedStackTrace = t.elementAt(1);
            el0 = t.elementAt(0);
        }
        final OtpErlangList erlStackFrames = (OtpErlangList) el0;
        final OtpErlangList bs = (OtpErlangList) stackAndBindings.elementAt(1);
        setStackFrames(module, line, erlStackFrames, bs);
        if (savedStackTrace instanceof OtpErlangTuple) {
            addStackTrace((OtpErlangTuple) savedStackTrace);
        }
    }

    private void addStackTrace(final OtpErlangTuple savedStackTrace) {
        final OtpErlangTuple t = (OtpErlangTuple) savedStackTrace.elementAt(2);
        final OtpErlangTuple t2 = (OtpErlangTuple) t.elementAt(1);
        final OtpErlangList stackTrace = (OtpErlangList) t2.elementAt(1);
        for (int i = 1, n = stackTrace.arity(); i < n; ++i) {
            final OtpErlangTuple frame = (OtpErlangTuple) stackTrace
                    .elementAt(i);
            final OtpErlangAtom m = (OtpErlangAtom) frame.elementAt(0);
            final OtpErlangAtom f = (OtpErlangAtom) frame.elementAt(1);
            final OtpErlangLong a = (OtpErlangLong) frame.elementAt(2);
            try {
                stackFrames.add(new ErlangUninterpretedStackFrame(
                        m.atomValue(), new ErlangFunction(f.atomValue(), a
                                .intValue()), this, getDebugTarget()));
            } catch (final OtpErlangRangeException e) {
                e.printStackTrace();
            }
        }
    }

    public void removeStackFrames() {
        stackFrames = new ArrayList<IStackFrame>();
    }

    public void setStackFrames(String module, int line,
            final OtpErlangList erlStackFrames, OtpErlangList bs) {
        stackFrames = new ArrayList<IStackFrame>();
        final IDebugTarget target = getDebugTarget();
        for (final OtpErlangObject o : erlStackFrames) {
            final OtpErlangTuple t = (OtpErlangTuple) o;
            final OtpErlangTuple ml = (OtpErlangTuple) t.elementAt(1);
            final OtpErlangObject ml0 = ml.elementAt(0);
            int stackFrameNo;
            final OtpErlangAtom m = (OtpErlangAtom) ml0;
            final OtpErlangLong l = (OtpErlangLong) ml.elementAt(1);
            final OtpErlangLong n = (OtpErlangLong) t.elementAt(3);
            try {
                stackFrameNo = n.intValue();
            } catch (final OtpErlangRangeException e) {
                stackFrameNo = -1;
            }
            stackFrames.add(new ErlangStackFrame(module, this, target, line,
                    null, bs, stackFrameNo));
            bs = (OtpErlangList) t.elementAt(2);
            module = m.atomValue();
            try {
                line = l.intValue();
            } catch (final OtpErlangRangeException e) {
                line = -1;
            }
        }
    }

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

    @Override
    public IStackFrame[] getStackFrames() throws DebugException {
        return stackFrames.toArray(new IStackFrame[stackFrames.size()]);
    }

    @Override
    public boolean hasStackFrames() throws DebugException {
        return !stackFrames.isEmpty();
    }

    @Override
    public int getPriority() throws DebugException {
        return 0;
    }

    @Override
    public IStackFrame getTopStackFrame() throws DebugException {
        if (stackFrames.isEmpty()) {
            return null;
        }
        return stackFrames.get(0);
    }

    @Override
    public String getName() throws DebugException {
        return toLocalPid(fPid);
    }

    public static String toLocalPid(final OtpErlangPid pid) {
        final int a1 = pid.id();
        final int a2 = pid.serial();
        return "<0." + a1 + "." + a2 + ">";
    }

    @Override
    public IBreakpoint[] getBreakpoints() {
        if (!stackFrames.isEmpty()) {
            final IStackFrame f = stackFrames.get(0);
            if (f instanceof ErlangStackFrame) {
                final ErlangStackFrame topFrame = (ErlangStackFrame) f;
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
                            ErlLogger.warn(e);
                        } catch (final CoreException e) {
                            ErlLogger.warn(e);
                        }
                    }
                }
            }
        }
        return new IBreakpoint[0];
    }

    @Override
    public boolean isSuspended() {
        // return getStatus().equals(STATUS_SUSPENDED)
        // || getStatus().equals(STATUS_BREAK)
        // || getStatus().equals(STATUS_IDLE);
        return getStatus().equals(STATUS_SUSPENDED)
                || getStatus().equals(STATUS_BREAK);
    }

    @Override
    public boolean isTerminated() {
        return getStatus().equals(STATUS_TERMINATED);
    }

    @Override
    public void resume() throws DebugException {
        stepping = false;
        ErlideDebug.resume(fBackend, getMeta());
    }

    @Override
    public void suspend() throws DebugException {
        stepping = false;
        ErlideDebug.suspend(fBackend, getMeta());
    }

    @Override
    public boolean canResume() {
        return isSuspended();
    }

    @Override
    public boolean canSuspend() {
        return !isSuspended();
    }

    @Override
    public boolean canStepInto() {
        return isSuspended();
    }

    @Override
    public boolean canStepOver() {
        return isSuspended();
    }

    @Override
    public boolean canStepReturn() {
        return isSuspended();
    }

    @Override
    public boolean isStepping() {
        return stepping;
    }

    public void setNotStepping() {
        stepping = false;
    }

    @Override
    public void stepInto() throws DebugException {
        stepping = true;
        ErlideDebug.stepInto(fBackend, getMeta());
    }

    @Override
    public void stepOver() throws DebugException {
        stepping = true;
        ErlideDebug.stepOver(fBackend, getMeta());
    }

    @Override
    public void stepReturn() throws DebugException {
        stepping = true;
        ErlideDebug.stepReturn(fBackend, getMeta());
    }

    @Override
    public boolean canTerminate() {
        return false;
    }

    @Override
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

    public void setExitStatus(final String fExitStatus) {
        this.fExitStatus = fExitStatus;
    }

    public String getExitStatus() {
        return fExitStatus;
    }

    public void setTracing(final boolean tracing) {
        if (fTracing != tracing) {
            fTracing = tracing;
            ErlideDebug.tracing(fBackend, tracing, getMeta());
        }
    }

    public boolean getTracing() {
        return fTracing;
    }

    public void dropToFrame(final int stackFrameNo) throws DebugException {
        if (!ErlideDebug.dropToFrame(fBackend, getMeta(), stackFrameNo)) {
            final IStatus s = new Status(IStatus.ERROR, ErlangCore.PLUGIN_ID,
                    DebugException.REQUEST_FAILED, "frame not found", null);
            throw new DebugException(s);
        }
    }

}

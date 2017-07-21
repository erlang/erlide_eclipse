package org.erlide.debug.ui.tracing;

import java.util.List;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;

import com.ericsson.otp.erlang.OtpErlangPid;

public class DebugTraceProcess extends DebugTraceElement implements IThread {
    private final DebugTraceTarget fTarget;
    private OtpErlangPid fPid;
    private List<IStackFrame> fStackFrames;

    public DebugTraceProcess(final IDebugTarget target, final OtpErlangPid pid) {
        super(target);
        fTarget = (DebugTraceTarget) target;
        fStackFrames = null;
        // new ArrayList<IStackFrame>();
        setPid(pid);
    }

    @Override
    public IBreakpoint[] getBreakpoints() {
        return null;
    }

    @Override
    public String getName() throws DebugException {
        return fPid.toString();
    }

    @Override
    public int getPriority() throws DebugException {
        return 0;
    }

    @Override
    public IStackFrame[] getStackFrames() throws DebugException {
        getStack();
        return fStackFrames.toArray(new IStackFrame[fStackFrames.size()]);
    }

    private void getStack() {
        if (fStackFrames == null) {
            final DebugTraceEvent event = fTarget.getCurrentEvent();
            fStackFrames = event.getStackFrames(fTarget, this);
        }
    }

    @Override
    public IStackFrame getTopStackFrame() throws DebugException {
        getStack();
        if (fStackFrames.isEmpty()) {
            return null;
        }
        return fStackFrames.get(0);
    }

    @Override
    public boolean hasStackFrames() throws DebugException {
        getStack();
        return !fStackFrames.isEmpty();
    }

    @Override
    public boolean canResume() {
        return false;
    }

    @Override
    public boolean canSuspend() {
        return true;
    }

    @Override
    public boolean isSuspended() {
        return true;
    }

    @Override
    public void resume() throws DebugException {
    }

    @Override
    public void suspend() throws DebugException {
    }

    @Override
    public boolean canStepInto() {
        return true;
    }

    @Override
    public boolean canStepOver() {
        return false;
    }

    @Override
    public boolean canStepReturn() {
        return false;
    }

    @Override
    public boolean isStepping() {
        return true;
    }

    @Override
    public void stepInto() throws DebugException {
        fStackFrames = null;
    }

    @Override
    public void stepOver() throws DebugException {
        fStackFrames = null;
    }

    @Override
    public void stepReturn() throws DebugException {
    }

    @Override
    public boolean canTerminate() {
        return false;
    }

    @Override
    public boolean isTerminated() {
        return false;
    }

    @Override
    public void terminate() throws DebugException {
    }

    public void setPid(final OtpErlangPid fPid) {
        this.fPid = fPid;
    }

    public OtpErlangPid getPid() {
        return fPid;
    }

}

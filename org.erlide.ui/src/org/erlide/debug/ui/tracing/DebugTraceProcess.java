package org.erlide.debug.ui.tracing;

import java.util.ArrayList;
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
		new ArrayList<IStackFrame>();
		setPid(pid);
	}

	public IBreakpoint[] getBreakpoints() {
		return null;
	}

	public String getName() throws DebugException {
		return fPid.toString();
	}

	public int getPriority() throws DebugException {
		return 0;
	}

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

	public IStackFrame getTopStackFrame() throws DebugException {
		getStack();
		if (fStackFrames.isEmpty()) {
			return null;
		}
		return fStackFrames.get(0);
	}

	public boolean hasStackFrames() throws DebugException {
		getStack();
		return !fStackFrames.isEmpty();
	}

	public boolean canResume() {
		return false;
	}

	public boolean canSuspend() {
		return true;
	}

	public boolean isSuspended() {
		return true;
	}

	public void resume() throws DebugException {
	}

	public void suspend() throws DebugException {
	}

	public boolean canStepInto() {
		return true;
	}

	public boolean canStepOver() {
		return false;
	}

	public boolean canStepReturn() {
		return false;
	}

	public boolean isStepping() {
		return true;
	}

	public void stepInto() throws DebugException {
		fStackFrames = null;
	}

	public void stepOver() throws DebugException {
		fStackFrames = null;
	}

	public void stepReturn() throws DebugException {
	}

	public boolean canTerminate() {
		return false;
	}

	public boolean isTerminated() {
		return false;
	}

	public void terminate() throws DebugException {
	}

	public void setPid(final OtpErlangPid fPid) {
		this.fPid = fPid;
	}

	public OtpErlangPid getPid() {
		return fPid;
	}

}

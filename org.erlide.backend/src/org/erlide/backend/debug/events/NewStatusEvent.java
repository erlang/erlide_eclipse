package org.erlide.backend.debug.events;

import org.eclipse.debug.core.DebugEvent;
import org.erlide.backend.debug.model.ErlangDebugTarget;
import org.erlide.backend.debug.model.ErlangProcess;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class NewStatusEvent extends IntEvent {

    public NewStatusEvent(final OtpErlangObject[] cmds) {
        super(cmds);
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget) {
        final OtpErlangPid pid = (OtpErlangPid) cmds[1];
        final ErlangProcess erlangProcess = debugTarget.getOrCreateErlangProcess(pid);
        final OtpErlangAtom sa = (OtpErlangAtom) cmds[2];
        final String status = sa.atomValue();
        if (status.equals("break")) {
            handleBreakStatus(erlangProcess, status);
        } else if (status.equals("exit")) {
            handleExitStatus(erlangProcess, status);
        } else if (status.equals("running")) {
            handleRunningStatus(erlangProcess, status);
        } else if (status.equals("idle")) {
            handleIdleStatus(erlangProcess, status);
        } else {
            erlangProcess.setStatus(status);
            erlangProcess.fireChangeEvent(DebugEvent.STATE | DebugEvent.CHANGE);
        }
    }

    private void handleIdleStatus(final ErlangProcess erlangProcess, final String status) {
        // FIXME: this must be cleaned, but the status messages seem
        // to come out of order...
        // erlangProcess.removeStackFrames();
        erlangProcess.setStatus(status);
        erlangProcess.fireChangeEvent(DebugEvent.STATE | DebugEvent.CHANGE);
    }

    private void handleRunningStatus(final ErlangProcess erlangProcess,
            final String status) {
        erlangProcess.setStatus(status);
        if (erlangProcess.isStepping()) {
            erlangProcess.fireResumeEvent(DebugEvent.STEP_OVER);
        } else {
            erlangProcess.fireResumeEvent(DebugEvent.RESUME);
        }
    }

    private void handleExitStatus(final ErlangProcess erlangProcess, final String status) {
        erlangProcess.setStatus(status);
        final OtpErlangObject esa = cmds[3];
        erlangProcess.setExitStatus(esa.toString());
        erlangProcess.fireSuspendEvent(DebugEvent.TERMINATE);
    }

    private void handleBreakStatus(final ErlangProcess erlangProcess, final String status) {
        erlangProcess.setStatus(status);
        if (!erlangProcess.isStepping()) {
            erlangProcess.fireSuspendEvent(DebugEvent.BREAKPOINT);
        }
    }

}

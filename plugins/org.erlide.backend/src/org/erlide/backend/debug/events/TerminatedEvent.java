package org.erlide.backend.debug.events;

import org.eclipse.debug.core.DebugException;
import org.erlide.backend.debug.model.ErlangDebugTarget;

import com.ericsson.otp.erlang.OtpErlangPid;

public class TerminatedEvent implements DebuggerEvent {

    @SuppressWarnings("unused")
    private final OtpErlangPid pid;

    public TerminatedEvent(final OtpErlangPid pid) {
        this.pid = pid;
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget) throws DebugException {
        debugTarget.terminate();
    }

}

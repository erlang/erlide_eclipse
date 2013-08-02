package org.erlide.backend.debug.events;

import org.erlide.backend.debug.model.ErlangDebugTarget;

import com.ericsson.otp.erlang.OtpErlangObject;

public class IntEvent implements DebuggerEvent {

    protected final OtpErlangObject[] cmds;

    public IntEvent(final OtpErlangObject[] cmds) {
        this.cmds = cmds;
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget) {
        // debugTarget.handleIntEvent(cmd);
    }

}

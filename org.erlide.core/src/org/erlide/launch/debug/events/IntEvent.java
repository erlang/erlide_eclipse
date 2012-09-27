package org.erlide.launch.debug.events;

import org.erlide.launch.debug.model.ErlangDebugTarget;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class IntEvent implements DebuggerEvent {

    private final OtpErlangObject cmd;

    public IntEvent(final OtpErlangObject cmd) {
        this.cmd = cmd;
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget) {
        debugTarget.handleIntEvent((OtpErlangTuple) cmd);
    }

}

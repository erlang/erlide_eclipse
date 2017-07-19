package org.erlide.backend.debug.events;

import org.erlide.backend.debug.model.ErlangDebugTarget;

import com.ericsson.otp.erlang.OtpErlangObject;

public class NewBreakEvent extends IntEvent {

    public NewBreakEvent(final OtpErlangObject[] cmds) {
        super(cmds);
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget) {
    }

}

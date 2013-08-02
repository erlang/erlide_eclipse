package org.erlide.backend.debug.events;

import org.erlide.backend.debug.model.ErlangDebugTarget;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;

public class UnknownEvent implements DebuggerEvent {

    private final OtpErlangObject message;

    public UnknownEvent(final OtpErlangObject message) {
        this.message = message;
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget) {
        ErlLogger.warn("Unknown debugger event: %s)", message);
    }
}

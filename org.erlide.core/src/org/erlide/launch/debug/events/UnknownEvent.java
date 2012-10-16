package org.erlide.launch.debug.events;

import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.debug.model.ErlangDebugTarget;

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

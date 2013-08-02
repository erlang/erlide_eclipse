package org.erlide.backend.debug;

import java.util.Collection;

import org.eclipse.debug.core.DebugException;
import org.erlide.backend.debug.events.DebuggerEvent;
import org.erlide.backend.debug.events.DebuggerEventFactory;
import org.erlide.backend.debug.model.ErlangDebugTarget;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;

class DebugEventHandler {

    private final ErlangDebugTarget debugTarget;

    DebugEventHandler(final ErlangDebugTarget erlangDebugTarget) {
        debugTarget = erlangDebugTarget;
    }

    public void handleMessages(final Collection<OtpErlangObject> messages)
            throws Exception {
        for (final OtpErlangObject message : messages) {
            try {
                handleMessage(message);
            } catch (final Throwable e) {
                ErlLogger.info(e);
            }
        }
    }

    private void handleMessage(final OtpErlangObject message)
            throws DebugException {
        // ErlLogger.debug("DEBUGGER @@@ >> " + message);
        final DebuggerEvent event = DebuggerEventFactory.parse(message);
        // ErlLogger.debug("DEBUGGER @@@ :: " + event);
        event.execute(debugTarget);
    }
}

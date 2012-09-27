package org.erlide.launch.debug;

import java.util.Collection;

import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.debug.events.DebuggerEvent;
import org.erlide.launch.debug.events.DebuggerEventFactory;
import org.erlide.launch.debug.model.ErlangDebugTarget;

import com.ericsson.otp.erlang.OtpErlangObject;

class DebugEventHandler {

    private final ErlangDebugTarget debugTarget;

    DebugEventHandler(final ErlangDebugTarget erlangDebugTarget) {
        debugTarget = erlangDebugTarget;
    }

    public void handleMessages(final Collection<OtpErlangObject> messages)
            throws Exception {
        for (final OtpErlangObject message : messages) {
            handleMessage(message);
        }
    }

    private void handleMessage(final OtpErlangObject message) throws Exception {
        // TODO More events from erlide_dbg_mon...
        ErlLogger.debug("@@@>> " + message);
        try {
            final DebuggerEvent event = DebuggerEventFactory.parse(message);
            event.execute(debugTarget);
        } catch (final Throwable e) {
            ErlLogger.info(e);
        }
    }
}

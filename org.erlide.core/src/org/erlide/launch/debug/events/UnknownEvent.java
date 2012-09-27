package org.erlide.launch.debug.events;

import org.eclipse.debug.core.DebugException;
import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.debug.model.ErlangDebugTarget;

public class UnknownEvent implements DebuggerEvent {

    private final Bindings b;

    public UnknownEvent(final Bindings b) {
        this.b = b;
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget)
            throws DebugException {
        ErlLogger.warn("Unknown debugger event: %s", b.toString());
    }

}

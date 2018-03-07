package org.erlide.backend.debug.events;

import org.eclipse.debug.core.DebugException;
import org.erlide.backend.debug.model.ErlangDebugTarget;

public interface DebuggerEvent {

    void execute(ErlangDebugTarget debugTarget) throws DebugException;

}

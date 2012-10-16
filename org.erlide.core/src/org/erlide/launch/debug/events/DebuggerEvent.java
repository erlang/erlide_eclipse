package org.erlide.launch.debug.events;

import org.eclipse.debug.core.DebugException;
import org.erlide.launch.debug.model.ErlangDebugTarget;

public interface DebuggerEvent {

    void execute(ErlangDebugTarget debugTarget) throws DebugException;

}

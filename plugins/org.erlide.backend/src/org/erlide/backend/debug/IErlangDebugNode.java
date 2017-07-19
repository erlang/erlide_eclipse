package org.erlide.backend.debug;

import org.erlide.backend.debug.model.ErlangDebugTarget;
import org.erlide.backend.debug.model.ErlangProcess;

public interface IErlangDebugNode {
    void addErlangProcess(ErlangProcess p);

    void removeErlangProcess(ErlangProcess p);

    ErlangDebugTarget getErlangDebugTarget();
}

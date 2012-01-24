package org.erlide.debug;

import org.erlide.debug.model.ErlangDebugTarget;
import org.erlide.debug.model.ErlangProcess;

public interface IErlangDebugNode {
    void addErlangProcess(ErlangProcess p);

    void removeErlangProcess(ErlangProcess p);

    ErlangDebugTarget getErlangDebugTarget();
}

package org.erlide.launch.debug;

import org.erlide.launch.debug.model.ErlangDebugTarget;
import org.erlide.launch.debug.model.ErlangProcess;

public interface IErlangDebugNode {
    void addErlangProcess(ErlangProcess p);

    void removeErlangProcess(ErlangProcess p);

    ErlangDebugTarget getErlangDebugTarget();
}

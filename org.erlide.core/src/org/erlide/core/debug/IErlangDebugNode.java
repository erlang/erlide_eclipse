package org.erlide.core.debug;

public interface IErlangDebugNode {
    void addErlangProcess(ErlangProcess p);

    void removeErlangProcess(ErlangProcess p);

    ErlangDebugTarget getErlangDebugTarget();
}

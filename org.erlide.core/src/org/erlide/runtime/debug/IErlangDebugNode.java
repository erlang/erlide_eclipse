package org.erlide.runtime.debug;

public interface IErlangDebugNode {
    void addErlangProcess(ErlangProcess p);

    void removeErlangProcess(ErlangProcess p);

    ErlangDebugTarget getErlangDebugTarget();
}

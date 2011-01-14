package org.erlide.runtime.debug;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IMemoryBlock;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IThread;

/**
 * A debug node is a distributed node connected to the debug target. It is only
 * created when debugging distributed systems, and only for the "other" nodes
 * connected.
 * 
 * @author jakob
 * 
 */
public class ErlangDebugNode extends ErlangDebugElement implements
        IDebugTarget, IErlangDebugNode {

    private final String fNodeName;
    private final List<ErlangProcess> fShownProcesses;
    private final ErlangDebugTarget fErlangDebugTarget;

    public ErlangDebugNode(final ErlangDebugTarget target, final String nodeName) {
        super(null);
        fNodeName = nodeName;
        fShownProcesses = new ArrayList<ErlangProcess>();
        fErlangDebugTarget = target;
    }

    public String getName() throws DebugException {
        return fNodeName;
    }

    public IProcess getProcess() {
        return null;
    }

    public IThread[] getThreads() throws DebugException {
        if (isTerminated()) {
            return ErlangDebugTarget.NO_PROCS;
        }
        return fShownProcesses.toArray(new IThread[fShownProcesses.size()]);
    }

    public boolean hasThreads() throws DebugException {
        return true;
    }

    public boolean supportsBreakpoint(final IBreakpoint breakpoint) {
        return fErlangDebugTarget.supportsBreakpoint(breakpoint);
    }

    public boolean canTerminate() {
        return true;
    }

    public boolean isTerminated() {
        return false;
    }

    public void terminate() throws DebugException {
        fErlangDebugTarget.terminate();
    }

    public boolean canResume() {
        return false;
    }

    public boolean canSuspend() {
        return false;
    }

    public boolean isSuspended() {
        return false;
    }

    public void resume() throws DebugException {
    }

    public void suspend() throws DebugException {
    }

    public void breakpointAdded(final IBreakpoint breakpoint) {
        fErlangDebugTarget.breakpointAdded(breakpoint);
    }

    public void breakpointChanged(final IBreakpoint breakpoint,
            final IMarkerDelta delta) {
        fErlangDebugTarget.breakpointChanged(breakpoint, delta);
    }

    public void breakpointRemoved(final IBreakpoint breakpoint,
            final IMarkerDelta delta) {
        fErlangDebugTarget.breakpointRemoved(breakpoint, delta);
    }

    public boolean canDisconnect() {
        return false;
    }

    public void disconnect() throws DebugException {
    }

    public boolean isDisconnected() {
        return false;
    }

    public IMemoryBlock getMemoryBlock(final long startAddress,
            final long length) throws DebugException {
        return fErlangDebugTarget.getMemoryBlock(startAddress, length);
    }

    public boolean supportsStorageRetrieval() {
        return fErlangDebugTarget.supportsStorageRetrieval();
    }

    public void addErlangProcess(final ErlangProcess ep) {
        fShownProcesses.add(ep);
    }

    public void removeErlangProcess(final ErlangProcess p) {
        fShownProcesses.remove(p);
    }

    @Override
    public ErlangDebugTarget getErlangDebugTarget() {
        return fErlangDebugTarget;
    }

    @Override
    public ILaunch getLaunch() {
        return fErlangDebugTarget.getLaunch();
    }

    @Override
    public IDebugTarget getDebugTarget() {
        return this;
    }
}

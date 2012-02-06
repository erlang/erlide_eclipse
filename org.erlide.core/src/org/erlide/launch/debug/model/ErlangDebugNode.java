package org.erlide.launch.debug.model;

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
import org.erlide.launch.debug.IErlangDebugNode;

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

    @Override
    public String getName() throws DebugException {
        return fNodeName;
    }

    @Override
    public IProcess getProcess() {
        return null;
    }

    @Override
    public IThread[] getThreads() throws DebugException {
        if (isTerminated()) {
            return ErlangDebugTarget.NO_PROCS;
        }
        return fShownProcesses.toArray(new IThread[fShownProcesses.size()]);
    }

    @Override
    public boolean hasThreads() throws DebugException {
        return true;
    }

    @Override
    public boolean supportsBreakpoint(final IBreakpoint breakpoint) {
        return fErlangDebugTarget.supportsBreakpoint(breakpoint);
    }

    @Override
    public boolean canTerminate() {
        return true;
    }

    @Override
    public boolean isTerminated() {
        return false;
    }

    @Override
    public void terminate() throws DebugException {
        fErlangDebugTarget.terminate();
    }

    @Override
    public boolean canResume() {
        return false;
    }

    @Override
    public boolean canSuspend() {
        return false;
    }

    @Override
    public boolean isSuspended() {
        return false;
    }

    @Override
    public void resume() throws DebugException {
    }

    @Override
    public void suspend() throws DebugException {
    }

    @Override
    public void breakpointAdded(final IBreakpoint breakpoint) {
        fErlangDebugTarget.breakpointAdded(breakpoint);
    }

    @Override
    public void breakpointChanged(final IBreakpoint breakpoint,
            final IMarkerDelta delta) {
        fErlangDebugTarget.breakpointChanged(breakpoint, delta);
    }

    @Override
    public void breakpointRemoved(final IBreakpoint breakpoint,
            final IMarkerDelta delta) {
        fErlangDebugTarget.breakpointRemoved(breakpoint, delta);
    }

    @Override
    public boolean canDisconnect() {
        return false;
    }

    @Override
    public void disconnect() throws DebugException {
    }

    @Override
    public boolean isDisconnected() {
        return false;
    }

    @Override
    public IMemoryBlock getMemoryBlock(final long startAddress,
            final long length) throws DebugException {
        return fErlangDebugTarget.getMemoryBlock(startAddress, length);
    }

    @Override
    public boolean supportsStorageRetrieval() {
        return fErlangDebugTarget.supportsStorageRetrieval();
    }

    @Override
    public void addErlangProcess(final ErlangProcess ep) {
        fShownProcesses.add(ep);
    }

    @Override
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

package org.erlide.debug.ui.tracing;

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
import org.erlide.launch.DummyProcess;

import com.ericsson.otp.erlang.OtpErlangPid;

public class DebugTraceTarget extends DebugTraceElement implements IDebugTarget {

    private final ILaunch launch;
    private final ILaunch parentLaunch;
    private final IDebugTarget node;
    private final List<DebugTraceProcess> processes;
    private final List<DebugTraceEvent> events;
    private final int pointer;
    private DebugTraceProcess currentProcess;
    private boolean fTerminated;
    private DummyProcess fDummyProcess;

    public DebugTraceTarget(final ILaunch launch, final ILaunch parentLaunch,
            final IDebugTarget node, final List<DebugTraceEvent> events) {
        super(null);
        this.launch = launch;
        this.parentLaunch = parentLaunch;
        this.node = node;
        this.events = events;
        processes = new ArrayList<DebugTraceProcess>();
        pointer = 0;
        fTerminated = false;
        cacheCurrentProcess();
    }

    private void cacheCurrentProcess() {
        final DebugTraceEvent event = getCurrentEvent();
        final OtpErlangPid pid = event.getPid();
        currentProcess = getProcessWithPid(pid);
        if (currentProcess == null) {
            currentProcess = new DebugTraceProcess(this, pid);
            processes.add(currentProcess);
        }
    }

    private DebugTraceProcess getProcessWithPid(final OtpErlangPid pid) {
        for (final DebugTraceProcess i : processes) {
            if (i.getPid().equals(pid)) {
                return i;
            }
        }
        return null;
    }

    public DebugTraceEvent getCurrentEvent() {
        return events.get(pointer);
    }

    @Override
    public String getName() throws DebugException {
        return launch.toString();
    }

    @Override
    public IProcess getProcess() {
        if (fDummyProcess == null) {
            fDummyProcess = new DummyProcess(getLaunch());
        }
        return fDummyProcess;
    }

    @Override
    public IThread[] getThreads() throws DebugException {
        return processes.toArray(new IThread[processes.size()]);
    }

    @Override
    public boolean hasThreads() throws DebugException {
        return !processes.isEmpty();
    }

    @Override
    public boolean supportsBreakpoint(final IBreakpoint breakpoint) {
        return false;
    }

    @Override
    public IDebugTarget getDebugTarget() {
        return this;
    }

    @Override
    public ILaunch getLaunch() {
        return launch;
    }

    @Override
    public boolean canTerminate() {
        return true;
    }

    @Override
    public boolean isTerminated() {
        return fTerminated;
    }

    @Override
    public void terminate() throws DebugException {
        fTerminated = true;
    }

    @Override
    public boolean canResume() {
        return true;
    }

    @Override
    public boolean canSuspend() {
        return true;
    }

    @Override
    public boolean isSuspended() {
        return true;
    }

    @Override
    public void resume() throws DebugException {
    }

    @Override
    public void suspend() throws DebugException {
    }

    @Override
    public void breakpointAdded(final IBreakpoint breakpoint) {
    }

    @Override
    public void breakpointChanged(final IBreakpoint breakpoint,
            final IMarkerDelta delta) {
    }

    @Override
    public void breakpointRemoved(final IBreakpoint breakpoint,
            final IMarkerDelta delta) {
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
        return null;
    }

    @Override
    public boolean supportsStorageRetrieval() {
        return false;
    }

}

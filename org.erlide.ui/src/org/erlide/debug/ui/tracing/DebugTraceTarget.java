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
import org.erlide.core.debug.DummyProcess;

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

    public String getName() throws DebugException {
        return launch.toString();
    }

    public IProcess getProcess() {
        if (fDummyProcess == null) {
            fDummyProcess = new DummyProcess(getLaunch());
        }
        return fDummyProcess;
    }

    public IThread[] getThreads() throws DebugException {
        return processes.toArray(new IThread[processes.size()]);
    }

    public boolean hasThreads() throws DebugException {
        return !processes.isEmpty();
    }

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

    public boolean canTerminate() {
        return true;
    }

    public boolean isTerminated() {
        return fTerminated;
    }

    public void terminate() throws DebugException {
        fTerminated = true;
    }

    public boolean canResume() {
        return true;
    }

    public boolean canSuspend() {
        return true;
    }

    public boolean isSuspended() {
        return true;
    }

    public void resume() throws DebugException {
    }

    public void suspend() throws DebugException {
    }

    public void breakpointAdded(final IBreakpoint breakpoint) {
    }

    public void breakpointChanged(final IBreakpoint breakpoint,
            final IMarkerDelta delta) {
    }

    public void breakpointRemoved(final IBreakpoint breakpoint,
            final IMarkerDelta delta) {
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
        return null;
    }

    public boolean supportsStorageRetrieval() {
        return false;
    }

}

package org.erlide.backend.debug;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IBreakpointsListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchesListener;
import org.eclipse.debug.core.model.IBreakpoint;
import org.erlide.util.ErlLogger;

public class ErlangDebugOptionsManager implements ILaunchesListener, IBreakpointsListener {

    public ErlangDebugOptionsManager() {
    }

    public void start() {
        // lazy initialization will occur on the first launch
        final DebugPlugin debugPlugin = DebugPlugin.getDefault();
        debugPlugin.getLaunchManager().addLaunchListener(this);
        debugPlugin.getBreakpointManager().addBreakpointListener(this);
        // EvaluationContextManager.startup();
    }

    public void shutdown() {
        final DebugPlugin debugPlugin = DebugPlugin.getDefault();
        debugPlugin.getLaunchManager().removeLaunchListener(this);
        debugPlugin.getBreakpointManager().removeBreakpointListener(this);
    }

    @Override
    public void launchesRemoved(final ILaunch[] launches) {
    }

    @Override
    public void launchesAdded(final ILaunch[] launches) {
    }

    @Override
    public void launchesChanged(final ILaunch[] launches) {
    }

    /**
     * Updates message attributes on the given erlang breakpoints.
     */
    private void updateBreakpointsMessages(final IBreakpoint[] breakpoints) {
        final IWorkspaceRunnable runnable = new IWorkspaceRunnable() {
            @Override
            public void run(final IProgressMonitor monitor) throws CoreException {
                for (final IBreakpoint breakpoint : breakpoints) {
                    if (breakpoint instanceof IErlangBreakpoint) {
                        final IErlangBreakpoint erlangBreakpoint = (IErlangBreakpoint) breakpoint;
                        final String message = erlangBreakpoint.getMessage();
                        breakpoint.getMarker().setAttribute(IMarker.MESSAGE, message);
                    }
                }
            }
        };
        try {
            ResourcesPlugin.getWorkspace().run(runnable, null, 0, null);
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }
    }

    @Override
    public void breakpointsAdded(final IBreakpoint[] breakpoints) {
        updateBreakpointsMessages(breakpoints);
    }

    @Override
    public void breakpointsRemoved(final IBreakpoint[] breakpoints,
            final IMarkerDelta[] deltas) {
    }

    @Override
    public void breakpointsChanged(final IBreakpoint[] breakpoints,
            final IMarkerDelta[] deltas) {
        updateBreakpointsMessages(breakpoints);
    }

}

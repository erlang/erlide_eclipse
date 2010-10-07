package org.erlide.runtime.debug;

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
import org.erlide.jinterface.util.ErlLogger;

public class ErlangDebugOptionsManager implements ILaunchesListener,
        IBreakpointsListener {
    private static ErlangDebugOptionsManager fgDebugOptionsManager;

    private ErlangDebugOptionsManager() {
    }

    public static ErlangDebugOptionsManager getDefault() {
        if (fgDebugOptionsManager == null) {
            fgDebugOptionsManager = new ErlangDebugOptionsManager();
        }
        return fgDebugOptionsManager;
    }

    public void startup() {
        // lazy initialization will occur on the first launch
        final DebugPlugin debugPlugin = DebugPlugin.getDefault();
        debugPlugin.getLaunchManager().addLaunchListener(this);
        debugPlugin.getBreakpointManager().addBreakpointListener(this);
        // EvaluationContextManager.startup();
    }

    public void launchesRemoved(final ILaunch[] launches) {
        // TODO Auto-generated method stub

    }

    public void launchesAdded(final ILaunch[] launches) {
        // TODO Auto-generated method stub

    }

    public void launchesChanged(final ILaunch[] launches) {
        // TODO Auto-generated method stub

    }

    /**
     * Updates message attributes on the given erlang breakpoints.
     */
    private void updateBreakpointsMessages(final IBreakpoint[] breakpoints) {
        final IWorkspaceRunnable runnable = new IWorkspaceRunnable() {
            public void run(final IProgressMonitor monitor)
                    throws CoreException {
                for (final IBreakpoint breakpoint : breakpoints) {
                    if (breakpoint instanceof IErlangBreakpoint) {
                        final IErlangBreakpoint erlangBreakpoint = (IErlangBreakpoint) breakpoint;
                        final String message = erlangBreakpoint.getMessage();
                        breakpoint.getMarker().setAttribute(IMarker.MESSAGE,
                                message);
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

    public void breakpointsAdded(final IBreakpoint[] breakpoints) {
        updateBreakpointsMessages(breakpoints);
    }

    public void breakpointsRemoved(final IBreakpoint[] breakpoints,
            final IMarkerDelta[] deltas) {
    }

    public void breakpointsChanged(final IBreakpoint[] breakpoints,
            final IMarkerDelta[] deltas) {
        updateBreakpointsMessages(breakpoints);
    }

}

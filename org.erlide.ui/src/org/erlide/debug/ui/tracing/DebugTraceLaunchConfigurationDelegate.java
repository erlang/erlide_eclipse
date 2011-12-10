package org.erlide.debug.ui.tracing;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.LaunchConfigurationDelegate;
import org.erlide.core.ErlangCore;

public class DebugTraceLaunchConfigurationDelegate extends
        LaunchConfigurationDelegate {
    private ILaunch parentLaunch;
    private IDebugTarget node;
    private List<DebugTraceEvent> events;

    public void setInfo(final ILaunch parentLaunch, final IDebugTarget node,
            final List<DebugTraceEvent> events) {
        this.parentLaunch = parentLaunch;
        this.node = node;
        this.events = events;
    }

    @Override
    public void launch(final ILaunchConfiguration configuration,
            final String mode, final ILaunch launch,
            final IProgressMonitor monitor) throws CoreException {
        if (!mode.equals(ILaunchManager.DEBUG_MODE)) {
            throw new CoreException(new Status(IStatus.ERROR,
                    ErlangCore.PLUGIN_ID, "debug mode not set"));
        }
        final DebugTraceTarget target = new DebugTraceTarget(launch,
                parentLaunch, node, events);
        launch.addDebugTarget(target);
    }

}

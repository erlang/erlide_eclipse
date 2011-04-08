package org.erlide.debug.ui.tracing;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchDelegate;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;
import org.erlide.debug.ui.views.DebuggerTraceView;
import org.erlide.jinterface.ErlLogger;

public class DebugTraceLaunching {

    public static void launch(final IDebugTarget target,
            final DebuggerTraceView debuggerTraceView) {
        if (target == null) {
            return;
        }
        try {
            final ILaunchConfigurationType lcType = DebugPlugin
                    .getDefault()
                    .getLaunchManager()
                    .getLaunchConfigurationType(
                            "org.erlide.runtime.debug.launchDebugTrace");
            final String name = target.toString();
            final ILaunchConfigurationWorkingCopy wc = lcType.newInstance(null,
                    name);
            final Set<String> modes = new HashSet<String>();
            modes.add(ILaunchManager.DEBUG_MODE);
            final ILaunchDelegate[] delegates = lcType.getDelegates(modes);
            final ILaunchConfigurationDelegate delegate = delegates[0]
                    .getDelegate();
            if (!(delegate instanceof DebugTraceLaunchConfigurationDelegate)) {
                return;
            }
            final DebugTraceLaunchConfigurationDelegate ldtlcd = (DebugTraceLaunchConfigurationDelegate) delegate;
            ldtlcd.setInfo(target.getLaunch(), target,
                    debuggerTraceView.getEventsForLaunch(target));
            wc.launch(ILaunchManager.DEBUG_MODE, null);
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }
    }
}

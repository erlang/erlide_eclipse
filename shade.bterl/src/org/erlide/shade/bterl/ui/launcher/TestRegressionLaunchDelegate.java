package org.erlide.shade.bterl.ui.launcher;

import java.io.File;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;
import org.eclipse.ui.PlatformUI;
import org.erlide.jinterface.ErlLogger;
import org.erlide.test_support.ui.suites.RegressionResultsView;

public class TestRegressionLaunchDelegate implements
        ILaunchConfigurationDelegate {

    @Override
    public void launch(final ILaunchConfiguration configuration,
            final String mode, final ILaunch launch,
            final IProgressMonitor monitor) throws CoreException {
        final String wdir = configuration.getAttribute(
                TestLaunchAttributes.WORKDIR, "");
        System.out.println("---@> launch regression in" + wdir);
        final File workdir = new File(wdir);
        if (!workdir.exists()) {
            ErlLogger.warn(
                    "Attempting to start bterl tests in missing directory %s",
                    workdir.getAbsolutePath());
            return;
        }
        final RegressionResultsView rview = (RegressionResultsView) PlatformUI
                .getWorkbench().getActiveWorkbenchWindow().getActivePage()
                .showView(RegressionResultsView.VIEW_ID);
        RegressionLauncher.getInstance().launch(wdir, monitor, rview);
    }

}

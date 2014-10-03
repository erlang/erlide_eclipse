package org.erlide.cover.runtime.launch;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.backend.api.BackendData;
import org.erlide.backend.launch.ErlangLaunchDelegate;
import org.erlide.cover.api.CoverException;
import org.erlide.cover.core.CoverBackend;
import org.erlide.cover.core.CoverRunner;
import org.erlide.util.ErlLogger;

/**
 * Cover launch configuration
 *
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 *
 */
public class CoverLaunchConfigurationDelegate extends ErlangLaunchDelegate {

    @Override
    public void launch(final ILaunchConfiguration config, final String mode,
            final ILaunch launch, final IProgressMonitor monitor) throws CoreException {

        try {
            super.launch(config, mode, launch, monitor);

            CoverLaunchData coverData;
            coverData = new CoverLaunchData(config);

            final CoverBackend coverBackend = CoverBackend.getInstance();
            coverBackend.setBackend(backend);
            coverBackend.initialize(coverData);
            coverBackend.runCoverageAnalysis(new CoverRunner());
        } catch (final CoreException e) {
            ErlLogger.error(e);
        } catch (final CoverException e) {
            if (CoverBackend.getInstance().getListeners().size() == 0) {
                throw new RuntimeException(e.getMessage());
            }
            CoverBackend.getInstance().handleError(e.getMessage());
        }
    }

    @Override
    protected BackendData configureBackend(final BackendData data, final String mode,
            final ILaunch launch) {
        final BackendData myData = super.configureBackend(data, mode, launch);
        myData.setConsole(true);
        myData.setLongName(false);
        myData.setReportErrors(true);
        myData.setNodeName(CoverBackend.NODE_NAME);
        myData.setUseStartShell(true);
        return myData;
    }

}

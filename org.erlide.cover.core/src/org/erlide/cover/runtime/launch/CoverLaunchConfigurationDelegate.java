package org.erlide.cover.runtime.launch;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.core.backend.Backend;
import org.erlide.core.backend.launching.ErlangLaunchDelegate;
import org.erlide.cover.api.CoverException;
import org.erlide.cover.core.CoverBackend;

/**
 * Launch cover configuration
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class CoverLaunchConfigurationDelegate extends ErlangLaunchDelegate {

    @Override
    public Backend doLaunch(final ILaunchConfiguration config, final String mode,
            final ILaunch launch, final IProgressMonitor monitor) {

        CoverLaunchData coverData;
        try {
            coverData = new CoverLaunchData(config);

            final CoverBackend coverBackend = CoverBackend.getInstance();
            coverBackend.initialize(coverData);
            coverBackend.startTesting();
            return coverBackend.getBackend();
        } catch (final CoreException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            return null;
        } catch (final CoverException e) {
            if (CoverBackend.getInstance().getListeners().size() == 0) {
                throw new RuntimeException(e.getMessage());
            }
            CoverBackend.getInstance().handleError(e.getMessage());
            return null;
        }

    }

}

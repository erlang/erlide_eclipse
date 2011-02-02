package org.erlide.cover.runtime.launch;

import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.cover.core.CoverBackend;
import org.erlide.jinterface.backend.Backend;
import org.erlide.runtime.launch.ErlLaunchData;
import org.erlide.runtime.launch.ErlangLaunchDelegate;

/**
 * Launch cover configuration
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class CoverLaunchConfigurationDelegate extends ErlangLaunchDelegate {

    @Override
    public void launch(final ILaunchConfiguration config, final String mode,
            final ILaunch launch, final IProgressMonitor monitor)
            throws CoreException {

        doLaunch(config, mode, launch, false, null);
    }

    @Override
    protected void doLaunch(final ILaunchConfiguration config,
            final String mode, final ILaunch launch, final boolean internal,
            final Map<String, String> env) throws CoreException {

        final CoverLaunchData coverData = new CoverLaunchData(config);
        final ErlLaunchData lData = new ErlLaunchData(config, internal);

        final CoverBackend coverBackend = CoverBackend.getInstance();
        coverBackend.initialize(lData, coverData);
        final Backend backend = coverBackend.getBackend();
        coverBackend.start();
    }

}

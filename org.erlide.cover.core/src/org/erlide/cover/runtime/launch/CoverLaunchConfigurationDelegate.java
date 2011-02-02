package org.erlide.cover.runtime.launch;

import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.cover.core.CoverBackend;
import org.erlide.cover.core.ICoverObserver;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.launch.ErlLaunchData;
import org.erlide.runtime.launch.ErlangLaunchConfigurationDelegate;

/**
 * Launch cover configuration
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class CoverLaunchConfigurationDelegate extends
        ErlangLaunchConfigurationDelegate {

    public void launch(ILaunchConfiguration config, String mode,
            ILaunch launch, IProgressMonitor monitor) throws CoreException {


        doLaunch(config, mode, launch, false, null);
    }

    protected Backend doLaunch(final ILaunchConfiguration config,
            final String mode, final ILaunch launch, final boolean internal,
            final Map<String, String> env) throws CoreException {

        CoverLaunchData coverData = new CoverLaunchData(config);
        ErlLaunchData lData = new ErlLaunchData(config, internal);


        Backend backend = null;

        CoverBackend coverBackend = CoverBackend.getInstance();
        coverBackend.initialize(lData, coverData);
        backend = coverBackend.getBackend();
        coverBackend.start();

        return backend;
    }

}

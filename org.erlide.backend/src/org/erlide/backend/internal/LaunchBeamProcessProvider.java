package org.erlide.backend.internal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IProcess;
import org.erlide.backend.api.BackendData;
import org.erlide.util.ErlLogger;
import org.erlide.util.IProvider;
import org.erlide.util.SystemConfiguration;

class LaunchBeamProcessProvider implements IProvider<IProcess> {

    private final BackendData data;

    LaunchBeamProcessProvider(final BackendData data) {
        this.data = data;
    }

    @Override
    public IProcess get() {
        ILaunch launch = data.getLaunch();
        if (launch == null) {
            launch = launchPeer();
            data.setLaunch(launch);
        }
        return launch.getProcesses().length == 0 ? null : launch.getProcesses()[0];
    }

    private ILaunch launchPeer() {
        final ILaunchConfiguration launchConfig = data.asLaunchConfiguration();
        try {
            final boolean registerForDebug = data.getLaunch() != null
                    || SystemConfiguration.getInstance().isDeveloper();
            return launchConfig.launch(ILaunchManager.RUN_MODE,
                    new NullProgressMonitor(), false, registerForDebug);
        } catch (final CoreException e) {
            ErlLogger.error(e);
            return null;
        }
    }

}

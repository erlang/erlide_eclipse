package org.erlide.cover.runtime.launch;

import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.core.backend.launching.ErlangLaunchDelegate;
import org.erlide.cover.core.CoverBackend;

/**
 * Launch cover configuration
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class CoverLaunchConfigurationDelegate extends ErlangLaunchDelegate {

    @Override
    public void doLaunch(final ILaunchConfiguration config,
            final String mode, final ILaunch launch) {

        CoverLaunchData coverData;
        try {
            coverData = new CoverLaunchData(config);
            
            final CoverBackend coverBackend = CoverBackend.getInstance();
            coverBackend.initialize(coverData);
            coverBackend.startTesting();
            
        } catch (CoreException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

}

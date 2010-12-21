package org.erlide.cover.runtime.launch;

import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.cover.core.CoverBackend;
import org.erlide.eunit.core.EUnitBackend;
import org.erlide.eunit.runtime.launch.EUnitLaunchData;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.launch.ErlLaunchData;
import org.erlide.runtime.launch.ErlangLaunchConfigurationDelegate;

public class CoverLaunchConfigurationDelegate extends 
        ErlangLaunchConfigurationDelegate
{
    
    public void launch(ILaunchConfiguration config, String mode,
            ILaunch launch, IProgressMonitor monitor) throws CoreException {
        
        System.out.println(this);
        
        doLaunch(config, mode, launch, false, null);
    }
    
    
    protected Backend doLaunch(final ILaunchConfiguration config,
             final String mode, final ILaunch launch, final boolean internal,
             final Map<String, String> env) throws CoreException {
         
        CoverLaunchData coverData = new CoverLaunchData(config);
        ErlLaunchData lData = new ErlLaunchData(config, internal);
         
         
    //  ILaunchConfiguration conf2 = prepareConfig(config, eunitData);
        
        Backend backend = null;
        ErlLogger.info("Launching...");
        System.out.println("LAUNCHING!!!!");
        try {
            
            //TODO: change to CoverBackend
            CoverBackend coverBackend = CoverBackend.getInstance();
            coverBackend.initialize(lData, coverData);
            System.out.println("Backend created");
            backend = coverBackend.getBackend();
            coverBackend.start();
            System.out.println("Started");
            
        } catch (RuntimeException e) {
            ErlLogger.error("Cannot obtain runtime info");
            e.printStackTrace();
        } catch (BackendException e) {
            ErlLogger.error("Cannot create backend");
            e.printStackTrace();
        }        
        
         
        return backend;
    }
     
}

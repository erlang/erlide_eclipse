package org.erlide.eunit.runtime.launch;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.erlide.eunit.core.EUnitBackend;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.debug.ErlangDebugTarget;
import org.erlide.runtime.launch.ErlLaunchAttributes;
import org.erlide.runtime.launch.ErlLaunchData;
import org.erlide.runtime.launch.ErlangLaunchConfigurationDelegate;

public class EUnitLaunchConfigurationDelegate extends 
		ErlangLaunchConfigurationDelegate {

	private ErlangDebugTarget target;
	
	public void launch(ILaunchConfiguration config, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		
		System.out.println(this);
		
		doLaunch(config, mode, launch, false, null);
	}
	
	
	protected Backend doLaunch(final ILaunchConfiguration config,
	         final String mode, final ILaunch launch, final boolean internal,
	         final Map<String, String> env) throws CoreException {
		 
		EUnitLaunchData eunitData = new EUnitLaunchData(config);
		ErlLaunchData lData = new ErlLaunchData(config, internal);
		 
		 
	//	ILaunchConfiguration conf2 = prepareConfig(config, eunitData);
		
		Backend backend = null;
		ErlLogger.info("Launching...");
		System.out.println("LAUNCHING!!!!");
		try {
			
			EUnitBackend eunitBackend = EUnitBackend.getInstance();
			eunitBackend.initialize(lData, eunitData);
			System.out.println("Backend created");
			backend = eunitBackend.getBackend();
			eunitBackend.start();
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

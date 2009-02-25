package org.erlide.testing.framework.launcher;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;
import org.erlide.testing.framework.TestFrameworkPlugin;


public class TestFrameworkLaunchConfigurationDelegate implements
		ILaunchConfigurationDelegate {

	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		System.out.println("LAUNCHING... " + configuration.getName() + " "
				+ mode + " " + launch.toString());

		System.out.println("Project: "
				+ configuration.getAttribute(
						TestFrameworkLaunchConfigurationConstants.ATTR_PROJECT_NAME,
						""));
		TestFrameworkPlugin.getModel().runTests(launch);
	}

}

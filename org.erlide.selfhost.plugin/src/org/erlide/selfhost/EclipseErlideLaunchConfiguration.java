package org.erlide.selfhost;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.pde.ui.launcher.EclipseApplicationLaunchConfiguration;
import org.erlide.runtime.backend.ErlLaunchAttributes;
import org.erlide.runtime.backend.ErlangLaunchConfigurationDelegate;

public class EclipseErlideLaunchConfiguration extends
		EclipseApplicationLaunchConfiguration {

	ErlangLaunchConfigurationDelegate erlDelegate = new ErlangLaunchConfigurationDelegate();

	@Override
	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		ILaunchConfigurationWorkingCopy conf = configuration.getWorkingCopy();
		String args = conf.getAttribute(
				"org.eclipse.jdt.launching.VM_ARGUMENTS", "");
		conf.setAttribute("org.eclipse.jdt.launching.VM_ARGUMENTS", args
				+ "\n-Derlide.label="
				+ configuration.getAttribute(ErlLaunchAttributes.NODE_NAME, "")
						.trim());
		super.launch(conf, mode, launch, monitor);

		erlDelegate.launchInternal(configuration, mode, launch, monitor);
	}
}

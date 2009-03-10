package org.erlide.selfhost.ui;

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

	@Override
	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		ILaunchConfigurationWorkingCopy conf = configuration.getWorkingCopy();
		String args = conf.getAttribute(
				"org.eclipse.jdt.launching.VM_ARGUMENTS", "");
		String nodeName = configuration.getAttribute(
				ErlLaunchAttributes.NODE_NAME, "").trim();
		args += "\n-Derlide.label=" + nodeName;
		conf.setAttribute("org.eclipse.jdt.launching.VM_ARGUMENTS", args);
		super.launch(conf, mode, launch, monitor);

		ErlangLaunchConfigurationDelegate erlDelegate = new ErlangLaunchConfigurationDelegate();
		erlDelegate.launchInternal(configuration, mode, launch, monitor);
	}
}

package org.erlide.selfhost.ui;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.pde.ui.launcher.EclipseApplicationLaunchConfiguration;
import org.erlide.runtime.launch.ErlLaunchAttributes;
import org.erlide.runtime.launch.ErlangLaunchConfigurationDelegate;

public class EclipseErlideLaunchConfiguration extends
EclipseApplicationLaunchConfiguration {

	@Override
	public void launch(final ILaunchConfiguration configuration, final String mode,
			final ILaunch launch, final IProgressMonitor monitor) throws CoreException {
		final ILaunchConfigurationWorkingCopy conf = configuration.getWorkingCopy();
		String args = conf.getAttribute(
				"org.eclipse.jdt.launching.VM_ARGUMENTS", "");
		final String nodeName = configuration.getAttribute(
				ErlLaunchAttributes.NODE_NAME, "").trim();
		args += "\n-Derlide.label=" + nodeName;
		conf.setAttribute("org.eclipse.jdt.launching.VM_ARGUMENTS", args);
		super.launch(conf, mode, launch, monitor);

		final ErlangLaunchConfigurationDelegate erlDelegate = new ErlangLaunchConfigurationDelegate();
		erlDelegate.launchInternal(configuration, mode, launch, monitor);
	}
}

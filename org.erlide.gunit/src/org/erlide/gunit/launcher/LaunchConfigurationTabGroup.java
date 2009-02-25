package org.erlide.gunit.launcher;

import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;

public class LaunchConfigurationTabGroup extends AbstractLaunchConfigurationTabGroup {

	public void createTabs(ILaunchConfigurationDialog dialog, String mode) {
		ILaunchConfigurationTab[] tabs= new ILaunchConfigurationTab[] {
			new LaunchConfigurationTab(),
		};
		setTabs(tabs);
		
	}

}

package org.erlide.testing.framework.launcher;

import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;

public class TestFrameworkLaunchConfigurationTabGroup extends AbstractLaunchConfigurationTabGroup {

	public void createTabs(ILaunchConfigurationDialog dialog, String mode) {
		ILaunchConfigurationTab[] tabs= new ILaunchConfigurationTab[] {
			new TestFrameworkLaunchConfigurationTab(),
		};
		setTabs(tabs);
		
	}

}

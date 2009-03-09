package org.erlide.selfhost.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;
import org.eclipse.pde.ui.launcher.EclipseLauncherTabGroup;
import org.erlide.ui.launch.ErlangNodeTabGroup;

public class ErlideLaunchConfigurationTabGroup extends EclipseLauncherTabGroup {

	public ErlideLaunchConfigurationTabGroup() {
	}

	@Override
	public void createTabs(ILaunchConfigurationDialog dialog, String mode) {
		super.createTabs(dialog, mode);
		ILaunchConfigurationTab[] tabs = getTabs();
		List<ILaunchConfigurationTab> both = new ArrayList<ILaunchConfigurationTab>(
				Arrays.asList(tabs));
		ErlangNodeTabGroup et = new ErlangNodeTabGroup();
		Collection<ILaunchConfigurationTab> newtabs = et.createMyTabs(dialog,
				mode);
		both.addAll(newtabs);
		setTabs(both.toArray(new ILaunchConfigurationTab[0]));
	}
}

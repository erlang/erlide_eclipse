package org.erlide.selfhost.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;
import org.eclipse.pde.ui.launcher.EclipseLauncherTabGroup;
import org.erlide.ui.launch.ErlangNodeTabGroup;

public class EclipseErlideLaunchConfigurationTabGroup extends
EclipseLauncherTabGroup {

	public EclipseErlideLaunchConfigurationTabGroup() {
	}

	@Override
	public void createTabs(final ILaunchConfigurationDialog dialog, final String mode) {
		super.createTabs(dialog, mode);
		final ILaunchConfigurationTab[] tabs = getTabs();
		final List<ILaunchConfigurationTab> both = new ArrayList<ILaunchConfigurationTab>(
				Arrays.asList(tabs));
		final int size = both.size();

		final ErlangNodeTabGroup et = new ErlangNodeTabGroup();
		final Collection<ILaunchConfigurationTab> newtabs = et.createMyTabs(dialog,
				mode);
		both.addAll(size - 2, newtabs);
		setTabs(both.toArray(new ILaunchConfigurationTab[0]));
	}
}

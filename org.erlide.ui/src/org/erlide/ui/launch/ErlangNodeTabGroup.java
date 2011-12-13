/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.launch;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.CommonTab;
import org.eclipse.debug.ui.EnvironmentTab;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;

public class ErlangNodeTabGroup extends AbstractLaunchConfigurationTabGroup {

    public Collection<ILaunchConfigurationTab> createMyTabs(
            final ILaunchConfigurationDialog dialog, final String mode) {
        ILaunchConfigurationTab[] tabs;
        if (mode.equals("debug")) {
            tabs = new ILaunchConfigurationTab[] { new ErlangMainTab(),
                    new RuntimeTab(), new DebugTab(), new CodepathTab() };
        } else {
            tabs = new ILaunchConfigurationTab[] { new ErlangMainTab(),
                    new RuntimeTab(), new CodepathTab() };
        }
        return Arrays.asList(tabs);
    }

    @Override
    public void createTabs(final ILaunchConfigurationDialog dialog,
            final String mode) {
        final List<ILaunchConfigurationTab> tabs = new ArrayList<ILaunchConfigurationTab>(
                createMyTabs(dialog, mode));
        tabs.addAll(Arrays.asList(new ILaunchConfigurationTab[] {
                new EnvironmentTab(), new CommonTab() }));
        setTabs(tabs.toArray(new ILaunchConfigurationTab[0]));
    }
}

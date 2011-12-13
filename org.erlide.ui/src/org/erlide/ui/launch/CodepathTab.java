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

import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

public class CodepathTab extends AbstractLaunchConfigurationTab {

    @Override
    public void createControl(final Composite parent) {

        final Composite comp = new Composite(parent, SWT.NONE);
        setControl(comp);
        final GridLayout topLayout = new GridLayout();
        comp.setLayout(topLayout);

        final Label notImplementedYetLabel = new Label(comp, SWT.NONE);
        final GridData gd_notImplementedYetLabel = new GridData(SWT.LEFT,
                SWT.TOP, true, true);
        notImplementedYetLabel.setLayoutData(gd_notImplementedYetLabel);
        notImplementedYetLabel.setText("Not implemented yet...");

    }

    @Override
    public void setDefaults(final ILaunchConfigurationWorkingCopy configuration) {
    }

    @Override
    public void initializeFrom(final ILaunchConfiguration configuration) {
    }

    @Override
    public void performApply(final ILaunchConfigurationWorkingCopy configuration) {
    }

    @Override
    public String getName() {
        return "Code path";
    }

    @Override
    public boolean isValid(final ILaunchConfiguration launchConfig) {
        return true;
    }

}

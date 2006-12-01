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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.erlide.runtime.backend.IErlangLaunchConfigurationAttributes;

public class ErlangMainTab extends AbstractLaunchConfigurationTab {

	private Text moduleText;

	private Text funcText;

	public void createControl(Composite parent) {
		final Font font = parent.getFont();

		final Composite comp = new Composite(parent, SWT.NONE);
		setControl(comp);
		final GridLayout topLayout = new GridLayout();
		comp.setLayout(topLayout);
		GridData gd;

		final Composite widthHeightNameComp = new Composite(comp, SWT.NONE);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		widthHeightNameComp.setLayoutData(gd);
		final GridLayout widthHeightNameLayout = new GridLayout();
		widthHeightNameLayout.marginHeight = 0;
		widthHeightNameLayout.marginWidth = 0;
		widthHeightNameLayout.numColumns = 4;
		widthHeightNameComp.setLayout(widthHeightNameLayout);

		final Label moduleLabel = new Label(widthHeightNameComp, SWT.NONE);
		moduleLabel.setText("module");
		moduleLabel.setFont(font);

		moduleText = new Text(widthHeightNameComp, SWT.SINGLE | SWT.BORDER);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		moduleText.setLayoutData(gd);
		moduleText.setFont(font);
		moduleText.addModifyListener(fBasicModifyListener);

		final Label funcLabel = new Label(widthHeightNameComp, SWT.NONE);
		funcLabel.setText("function");
		funcLabel.setFont(font);

		funcText = new Text(widthHeightNameComp, SWT.SINGLE | SWT.BORDER);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		funcText.setLayoutData(gd);
		funcText.setFont(font);
		funcText.addModifyListener(fBasicModifyListener);

	}

	public void setDefaults(ILaunchConfigurationWorkingCopy configuration) {
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_ENODE_MODULE,
				IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_MODULE);
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_ENODE_FUNCTION,
				IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_FUNCTION);
	}

	public void initializeFrom(ILaunchConfiguration configuration) {
		try {
			moduleText.setText(configuration.getAttribute(
					IErlangLaunchConfigurationAttributes.ATTR_ENODE_MODULE,
					IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_MODULE));
		} catch (final CoreException e) {
			moduleText
					.setText(IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_MODULE);
		}
		try {
			funcText
					.setText(configuration
							.getAttribute(
									IErlangLaunchConfigurationAttributes.ATTR_ENODE_FUNCTION,
									IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_FUNCTION));
		} catch (final CoreException e) {
			funcText
					.setText(IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_FUNCTION);
		}

	}

	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_ENODE_MODULE,
				moduleText.getText());
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_ENODE_FUNCTION,
				funcText.getText());
	}

	public String getName() {
		return "Erlang node";
	}

	@Override
	public boolean isValid(ILaunchConfiguration launchConfig) {
		return true;
	}

	private final ModifyListener fBasicModifyListener = new ModifyListener() {

		public void modifyText(ModifyEvent evt) {
			updateLaunchConfigurationDialog();
		}
	};

}

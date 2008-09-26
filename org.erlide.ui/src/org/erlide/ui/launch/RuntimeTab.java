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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.erlide.runtime.backend.IErlLaunchAttributes;
import org.erlide.runtime.backend.RuntimeInfo;
import org.erlide.runtime.backend.RuntimeInfoManager;
import org.erlide.ui.util.SWTUtil;

public class RuntimeTab extends AbstractLaunchConfigurationTab {

	private Combo runtimesCombo;
	private Button startNodeCheckbox;
	private Text cookieText;
	private Text nameText;

	private Collection<RuntimeInfo> runtimes;

	public void createControl(final Composite parent) {
		runtimes = RuntimeInfoManager.getDefault().getRuntimes();

		final Composite comp = new Composite(parent, SWT.NONE);
		setControl(comp);
		final GridLayout topLayout = new GridLayout();
		comp.setLayout(topLayout);

		final Group runtimeGroup = SWTUtil.createGroup(comp, "Runtime", 1,
				GridData.FILL_HORIZONTAL);
		final GridData gd_runtimeGroup = new GridData(SWT.FILL, SWT.CENTER,
				false, false);
		runtimeGroup.setLayoutData(gd_runtimeGroup);
		final GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 2;
		runtimeGroup.setLayout(gridLayout);
		new Label(runtimeGroup, SWT.NONE);

		final Label label = new Label(runtimeGroup, SWT.NONE);
		label.setText("       ");

		final Label runtimeLabel = new Label(runtimeGroup, SWT.NONE);
		runtimeLabel.setLayoutData(new GridData());
		runtimeLabel.setText("Runtime");

		List<String> rtl = new ArrayList<String>();
		for (RuntimeInfo r : runtimes) {
			rtl.add(r.getName());
		}
		String[] rts = rtl.toArray(new String[] {});
		RuntimeInfo defaultRuntime = RuntimeInfoManager.getDefault()
				.getDefaultRuntime();
		int db = defaultRuntime == null ? 0 : Arrays.binarySearch(rts,
				defaultRuntime.getName());

		runtimesCombo = new Combo(runtimeGroup, SWT.READ_ONLY);
		runtimesCombo.setLayoutData(new GridData(210, SWT.DEFAULT));
		runtimesCombo.addSelectionListener(new SelectionAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void widgetSelected(final SelectionEvent e) {
				updateLaunchConfigurationDialog();
			}
		});
		runtimesCombo.setItems(rts);
		runtimesCombo.select(db);

		final Label nodeNameLabel = new Label(runtimeGroup, SWT.NONE);
		nodeNameLabel.setText("Node name");

		nameText = new Text(runtimeGroup, SWT.BORDER);
		nameText.setLayoutData(new GridData(232, SWT.DEFAULT));
		nameText.addModifyListener(new ModifyListener() {
			@SuppressWarnings("synthetic-access")
			public void modifyText(final ModifyEvent e) {
				boolean isRemote = nameText.getText().contains("@");
				startNodeCheckbox.setEnabled(!isRemote);
				if (isRemote) {
					startNodeCheckbox.setSelection(false);
				}
				updateLaunchConfigurationDialog();
			}
		});

		final Label cookieLabel = new Label(runtimeGroup, SWT.NONE);
		cookieLabel.setToolTipText("Leave empty to use default one");
		cookieLabel.setText("Cookie");

		cookieText = new Text(runtimeGroup, SWT.BORDER);
		cookieText.setLayoutData(new GridData(232, SWT.DEFAULT));
		cookieText.addModifyListener(new ModifyListener() {
			@SuppressWarnings("synthetic-access")
			public void modifyText(final ModifyEvent e) {
				updateLaunchConfigurationDialog();
			}
		});
		cookieText.setToolTipText("Leave empty to use default one");
		new Label(runtimeGroup, SWT.NONE);

		startNodeCheckbox = new Button(runtimeGroup, SWT.CHECK);
		startNodeCheckbox.setLayoutData(new GridData());
		startNodeCheckbox.setSelection(true);
		startNodeCheckbox
				.setText("Start the Erlang node if not running already");
		startNodeCheckbox.addSelectionListener(new SelectionAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void widgetSelected(final SelectionEvent e) {
				updateLaunchConfigurationDialog();
			}
		});

		final Label aLabel = new Label(runtimeGroup, SWT.WRAP);
		aLabel.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false,
				2, 1));

	}

	public void setDefaults(final ILaunchConfigurationWorkingCopy config) {
		config.setAttribute(IErlLaunchAttributes.START_ME, false);
		config.setAttribute(IErlLaunchAttributes.RUNTIME_NAME,
				IErlLaunchAttributes.DEFAULT_RUNTIME_NAME);
		config.setAttribute(IErlLaunchAttributes.NODE_NAME, "");
		config.setAttribute(IErlLaunchAttributes.COOKIE, "");
	}

	public void initializeFrom(final ILaunchConfiguration config) {
		try {
			final String runtimeName = config.getAttribute(
					IErlLaunchAttributes.RUNTIME_NAME, "");
			runtimesCombo.select(runtimesCombo.indexOf(runtimeName));
		} catch (final CoreException e) {
			runtimesCombo.setText("");
		}
		try {
			String node = config.getAttribute(IErlLaunchAttributes.NODE_NAME,
					"");
			nameText.setText(node);
		} catch (CoreException e) {
			nameText.setText("");
		}
		try {
			String cookie = config
					.getAttribute(IErlLaunchAttributes.COOKIE, "");
			cookieText.setText(cookie);
		} catch (CoreException e) {
			cookieText.setText("");
		}
		try {
			boolean startMe = config.getAttribute(
					IErlLaunchAttributes.START_ME, false);
			startNodeCheckbox.setSelection(startMe);
		} catch (CoreException e) {
			startNodeCheckbox.setSelection(false);
		}
	}

	public void performApply(final ILaunchConfigurationWorkingCopy config) {
		config.setAttribute(IErlLaunchAttributes.RUNTIME_NAME, runtimesCombo
				.getText());
		config.setAttribute(IErlLaunchAttributes.START_ME, startNodeCheckbox
				.getSelection());
		config.setAttribute(IErlLaunchAttributes.NODE_NAME, nameText.getText());
		config.setAttribute(IErlLaunchAttributes.COOKIE, cookieText.getText());
	}

	public String getName() {
		return "Runtimes";
	}

	@Override
	public boolean isValid(final ILaunchConfiguration config) {
		if (!RuntimeInfo.validateNodeName(nameText.getText())) {
			return false;
		}
		return true;

	}
}

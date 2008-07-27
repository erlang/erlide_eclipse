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
package org.erlide.basicui.prefs;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.erlide.basicui.ErlideBasicUIPlugin;

import com.swtdesigner.ResourceManager;

public class ErlangPreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage {

	@Override
	protected Control createContents(final Composite parent) {
		noDefaultAndApplyButton();
		final Composite panel = new Composite(parent, SWT.NONE);
		final GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		panel.setLayout(layout);

		final Label img = new Label(panel, SWT.NONE);
		img.setLayoutData(new GridData(144, SWT.DEFAULT));
		img.setImage(ResourceManager.getPluginImage(ErlideBasicUIPlugin
				.getDefault(), "icons/erlang058.gif"));

		final Label text = new Label(panel, SWT.NONE);
		text.setLayoutData(new GridData(290, SWT.DEFAULT));
		text.setText(PreferenceMessages.getString("ErlangPreferencePage.2")); //$NON-NLS-1$

		// final SelectionListener linkListener = new SelectionListener() {
		// public void widgetSelected(SelectionEvent e) {
		// Program.launch(e.text);
		// }
		//
		// public void widgetDefaultSelected(SelectionEvent e) {
		// }
		// };

		final Composite panel2 = new Composite(panel, SWT.NONE);
		final GridData gd_panel2 = new GridData(SWT.LEFT, SWT.CENTER, false,
				false, 2, 1);
		gd_panel2.widthHint = 441;
		panel2.setLayoutData(gd_panel2);
		final GridLayout layout2 = new GridLayout();
		panel2.setLayout(layout2);

		final Button reportButton = new Button(panel2, SWT.NONE);
		reportButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				PreferencesUtil.createPreferenceDialogOn(getShell(),
						"org.erlide.basic.reporting", null, null);
			}
		});
		reportButton.setLayoutData(new GridData(SWT.RIGHT, SWT.BOTTOM, false,
				false));
		reportButton.setText("Report problems");

		final Group group = new Group(panel2, SWT.NONE);
		group.setLayoutData(new GridData(421, SWT.DEFAULT));
		group.setLayout(new GridLayout());
		new Label(group, SWT.NONE).setText("The main developers are: ");
		new Label(group, SWT.NONE).setText("  * Vlad Dumitrescu");
		new Label(group, SWT.NONE).setText("  * Jakob Cederlund");
		// new Label(group, SWT.NONE).setText(" * Tomas Daarstad");

		new Label(group, SWT.NONE).setText("Other contributors:");
		new Label(group, SWT.NONE).setText("  * Lukas Larsson");
		new Label(group, SWT.NONE).setText("  * Mickaël Rémond");

		final Label alsoManyThanksLabel = new Label(group, SWT.NONE);
		alsoManyThanksLabel
				.setText("also many thanks to all who gave their feedback and helped to improve Erlide! ");

		return panel;
	}

	public void init(final IWorkbench workbench) {
	}

}

/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs;

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
import org.eclipse.ui.dialogs.PropertyPage;
import org.erlide.core.builder.CompilerPreferences;
import org.erlide.jinterface.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

public class CompilerPreferencePage extends PropertyPage implements
		IWorkbenchPreferencePage {

	final CompilerPreferences prefs;

	public CompilerPreferencePage() {
		super();
		setTitle("Compiler options (not fully functional yet)");
		setDescription("Select the compiler options to be used.");
		prefs = new CompilerPreferences();
	}

	@Override
	protected Control createContents(final Composite parent) {
		final Composite control = new Composite(parent, SWT.NONE);
		final GridLayout gridLayout_1 = new GridLayout();
		gridLayout_1.numColumns = 2;
		control.setLayout(gridLayout_1);

		final Button debugInfoButton = new Button(control, SWT.CHECK);
		final GridData gd_debugInfoButton = new GridData(SWT.FILL, SWT.CENTER,
				false, false);
		gd_debugInfoButton.widthHint = 124;
		debugInfoButton.setLayoutData(gd_debugInfoButton);
		debugInfoButton.setText("Debug info");
		debugInfoButton.setSelection(prefs.hasDebugInfo());
		debugInfoButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				prefs.setDebugInfo(debugInfoButton.getSelection());
			}
		});

		final Button encryptDebugInfoButton = new Button(control, SWT.CHECK);
		encryptDebugInfoButton.setEnabled(false);
		encryptDebugInfoButton
				.setToolTipText("the key will be read from an .erlang.crypt file");
		final GridData gd_encryptDebugInfoButton = new GridData(SWT.FILL,
				SWT.CENTER, true, false);
		gd_encryptDebugInfoButton.heightHint = 23;
		encryptDebugInfoButton.setLayoutData(gd_encryptDebugInfoButton);
		encryptDebugInfoButton.setText("Encrypt debug info");

		final Button btnAddexportall = new Button(control, SWT.CHECK);
		btnAddexportall.setEnabled(true);
		btnAddexportall.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false,
				false, 1, 1));
		btnAddexportall.setText("Use 'export_all'");
		btnAddexportall.setSelection(prefs.useExportAll());
		btnAddexportall.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				prefs.setUseExportAll(btnAddexportall.getSelection());
			}
		});

		new Label(control, SWT.NONE);
		new Label(control, SWT.NONE);
		new Label(control, SWT.NONE);

		final Group group = new Group(control, SWT.NONE);
		group
				.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false,
						2, 1));
		group.setText("Warnings");
		final GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 2;
		group.setLayout(gridLayout);

		final Button formatStringsButton = new Button(group, SWT.CHECK);
		formatStringsButton.setEnabled(false);
		formatStringsButton
				.setToolTipText("Malformed format strings as arguments to io:format and similar functions.");
		formatStringsButton.setSelection(true);
		formatStringsButton.setText("invalid 'format' strings ");

		final Button deprecatedFunctionsButton = new Button(group, SWT.CHECK);
		deprecatedFunctionsButton.setEnabled(false);
		deprecatedFunctionsButton
				.setToolTipText("call to a function known by the compiler to be deprecated");
		deprecatedFunctionsButton.setSelection(true);
		deprecatedFunctionsButton.setText("deprecated functions");

		final Button nameClashesWithButton = new Button(group, SWT.CHECK);
		nameClashesWithButton.setEnabled(false);
		nameClashesWithButton
				.setToolTipText("an exported function with the same name as an auto-imported BIF (such as size/1)\n AND there is a call to it without a qualifying module name.");
		nameClashesWithButton.setSelection(true);
		nameClashesWithButton.setText("name clashes with builtins");

		final Button obsoleteGuatdsButton = new Button(group, SWT.CHECK);
		obsoleteGuatdsButton.setEnabled(false);
		obsoleteGuatdsButton
				.setToolTipText("calls to old type testing BIFs such as pid/1 and list/1");
		obsoleteGuatdsButton.setSelection(true);
		obsoleteGuatdsButton.setText("obsolete guards");

		final Button useOfExport_allButton = new Button(group, SWT.CHECK);
		useOfExport_allButton.setEnabled(false);
		useOfExport_allButton.setSelection(true);
		useOfExport_allButton.setText("use of export_all");

		final Button unusedImportsButton = new Button(group, SWT.CHECK);
		unusedImportsButton.setEnabled(false);
		unusedImportsButton.setToolTipText("unused imported functions");
		unusedImportsButton.setSelection(true);
		unusedImportsButton.setText("unused imports");

		final Button variablesExportedOutsideButton = new Button(group,
				SWT.CHECK);
		variablesExportedOutsideButton.setEnabled(false);
		variablesExportedOutsideButton
				.setToolTipText("implicitly exported variables referred to after the primitives where they were first defined");
		variablesExportedOutsideButton.setSelection(true);
		variablesExportedOutsideButton.setText("exported implicit variables");

		final Button unusedVariablesButton = new Button(group, SWT.CHECK);
		unusedVariablesButton.setEnabled(false);
		unusedVariablesButton
				.setToolTipText("variables which are not used, with the exception of variables beginning with an underscore");
		unusedVariablesButton.setSelection(true);
		unusedVariablesButton.setText("unused variables");

		final Button shadowedVariablesButton = new Button(group, SWT.CHECK);
		shadowedVariablesButton.setEnabled(false);
		shadowedVariablesButton
				.setToolTipText("\"fresh\" variables in functional objects or list comprehensions with the same name as some already defined variable");
		shadowedVariablesButton.setSelection(true);
		shadowedVariablesButton.setText("shadowed variables");

		final Button unusedRecordsButton = new Button(group, SWT.CHECK);
		unusedRecordsButton.setEnabled(false);
		unusedRecordsButton
				.setToolTipText("unused locally defined record types");
		unusedRecordsButton.setSelection(true);
		unusedRecordsButton.setText("unused records");

		final Button unusedFunctionsButton = new Button(group, SWT.CHECK);
		unusedFunctionsButton.setEnabled(false);
		unusedFunctionsButton
				.setToolTipText("local functions that are not called directly or indirectly by an exported function");
		unusedFunctionsButton.setSelection(true);
		unusedFunctionsButton.setText("unused functions");
		new Label(group, SWT.NONE);

		final Button warnForSourceButton = new Button(control, SWT.CHECK);
		warnForSourceButton.setEnabled(false);
		warnForSourceButton.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER,
				false, false, 2, 1));
		warnForSourceButton
				.setText("Warn for source files not on the project's source path");
		warnForSourceButton.setSelection(prefs.doWarnModuleNotOnSourcePath());

		return control;
	}

	@Override
	public boolean performOk() {
		try {
			prefs.store();
		} catch (BackingStoreException e) {
			ErlLogger.warn(e);
		}
		return super.performOk();
	}

	@Override
	protected void performDefaults() {
		super.performDefaults();
		try {
			prefs.load();
		} catch (final BackingStoreException e) {
			ErlLogger.warn(e);
		}
	}

	public void init(final IWorkbench workbench) {
		performDefaults();
	}
}

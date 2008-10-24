package org.erlide.ui.prefs;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.core.builder.CompilerPreferences;
import org.osgi.service.prefs.BackingStoreException;

public class CompilerPreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage {

	private CompilerPreferences prefs;

	public CompilerPreferencePage() {
		super();
		setTitle("Compiler options (not functional yet)");
		setDescription("Select the compiler options to be used.");
		prefs = new CompilerPreferences();
		performDefaults();
	}

	@Override
	protected Control createContents(Composite parent) {
		Composite control = new Composite(parent, SWT.NONE);
		control.setEnabled(false);
		final GridLayout gridLayout_1 = new GridLayout();
		gridLayout_1.numColumns = 2;
		control.setLayout(gridLayout_1);

		final Button debugInfoButton = new Button(control, SWT.CHECK);
		debugInfoButton.setSelection(true);
		final GridData gd_debugInfoButton = new GridData(SWT.FILL, SWT.CENTER,
				false, false);
		gd_debugInfoButton.widthHint = 124;
		debugInfoButton.setLayoutData(gd_debugInfoButton);
		debugInfoButton.setText("Debug info");

		final Button encryptDebugInfoButton = new Button(control, SWT.CHECK);
		encryptDebugInfoButton.setEnabled(false);
		encryptDebugInfoButton
				.setToolTipText("the key will be read from an .erlang.crypt file");
		final GridData gd_encryptDebugInfoButton = new GridData(SWT.FILL,
				SWT.CENTER, true, false);
		gd_encryptDebugInfoButton.heightHint = 23;
		encryptDebugInfoButton.setLayoutData(gd_encryptDebugInfoButton);
		encryptDebugInfoButton.setText("Encrypt debug info");

		final Group group = new Group(control, SWT.NONE);
		group
				.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false,
						2, 1));
		group.setText("Warnings");
		final GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 2;
		group.setLayout(gridLayout);

		final Button formatStringsButton = new Button(group, SWT.CHECK);
		formatStringsButton
				.setToolTipText("Malformed format strings as arguments to io:format and similar functions.");
		formatStringsButton.setSelection(true);
		formatStringsButton.setText("invalid 'format' strings ");

		final Button deprecatedFunctionsButton = new Button(group, SWT.CHECK);
		deprecatedFunctionsButton
				.setToolTipText("call to a function known by the compiler to be deprecated");
		deprecatedFunctionsButton.setSelection(true);
		deprecatedFunctionsButton.setText("deprecated functions");

		final Button nameClashesWithButton = new Button(group, SWT.CHECK);
		nameClashesWithButton
				.setToolTipText("an exported function with the same name as an auto-imported BIF (such as size/1)\n AND there is a call to it without a qualifying module name.");
		nameClashesWithButton.setSelection(true);
		nameClashesWithButton.setText("name clashes with builtins");

		final Button obsoleteGuatdsButton = new Button(group, SWT.CHECK);
		obsoleteGuatdsButton
				.setToolTipText("calls to old type testing BIFs such as pid/1 and list/1");
		obsoleteGuatdsButton.setSelection(true);
		obsoleteGuatdsButton.setText("obsolete guards");

		final Button useOfExport_allButton = new Button(group, SWT.CHECK);
		useOfExport_allButton.setSelection(true);
		useOfExport_allButton.setText("use of export_all");

		final Button unusedImportsButton = new Button(group, SWT.CHECK);
		unusedImportsButton.setToolTipText("unused imported functions");
		unusedImportsButton.setSelection(true);
		unusedImportsButton.setText("unused imports");

		final Button variablesExportedOutsideButton = new Button(group,
				SWT.CHECK);
		variablesExportedOutsideButton
				.setToolTipText("implicitly exported variables referred to after the primitives where they were first defined");
		variablesExportedOutsideButton.setSelection(true);
		variablesExportedOutsideButton.setText("exported implicit variables");

		final Button unusedVariablesButton = new Button(group, SWT.CHECK);
		unusedVariablesButton
				.setToolTipText("variables which are not used, with the exception of variables beginning with an underscore");
		unusedVariablesButton.setSelection(true);
		unusedVariablesButton.setText("unused variables");

		final Button shadowedVariablesButton = new Button(group, SWT.CHECK);
		shadowedVariablesButton
				.setToolTipText("\"fresh\" variables in functional objects or list comprehensions with the same name as some already defined variable");
		shadowedVariablesButton.setSelection(true);
		shadowedVariablesButton.setText("shadowed variables");

		final Button unusedRecordsButton = new Button(group, SWT.CHECK);
		unusedRecordsButton
				.setToolTipText("unused locally defined record types");
		unusedRecordsButton.setSelection(true);
		unusedRecordsButton.setText("unused records");

		final Button unusedFunctionsButton = new Button(group, SWT.CHECK);
		unusedFunctionsButton
				.setToolTipText("local functions that are not called directly or indirectly by an exported function");
		unusedFunctionsButton.setSelection(true);
		unusedFunctionsButton.setText("unused functions");
		new Label(group, SWT.NONE);

		final Button warnForSourceButton = new Button(control, SWT.CHECK);
		warnForSourceButton.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER,
				false, false, 2, 1));
		warnForSourceButton
				.setText("Warn for source file not on the project's source path");
		return control;
	}

	@Override
	public boolean performOk() {
		prefs.store();
		return super.performOk();
	}

	@Override
	protected void performDefaults() {
		super.performDefaults();
		try {
			prefs.load();
		} catch (BackingStoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void init(final IWorkbench workbench) {
	}
}

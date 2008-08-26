/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.properties;

import java.util.Arrays;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.PropertyPage;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.runtime.backend.RuntimeInfoManager;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.properties.internal.MockupPreferenceStore;

public class OldErlProjectPropertyPage extends PropertyPage implements
		IPropertyChangeListener {

	Combo buildBackendName;
	private ErlangProjectProperties prefs;
	private Text output;
	private Text source;
	private Text include;
	private MockupPreferenceStore mockPrefs;
	private Button uz;
	private Text externalIncludes;

	/**
	 * Constructor for ErlProjectPropertyPage.
	 */
	public OldErlProjectPropertyPage() {
		super();
	}

	/**
	 * @see PreferencePage#createContents(Composite)
	 */
	@Override
	protected Control createContents(Composite parent) {
		final IProject prj = (IProject) getElement().getAdapter(IProject.class);
		prefs = new ErlangProjectProperties(prj);
		mockPrefs = new MockupPreferenceStore();
		mockPrefs.addPropertyChangeListener(this);

		// create the composite to hold the widgets
		final Composite composite = new Composite(parent, SWT.NONE);

		// create the desired layout for this wizard page
		final GridLayout gl = new GridLayout();
		gl.numColumns = 3;
		composite.setLayout(gl);

		String resourceString = ErlideUIPlugin
				.getResourceString("wizards.labels.buildoutput");
		// create the widgets and their grid data objects
		final Label outLabel = new Label(composite, SWT.NONE);
		outLabel.setText("Output directory:");
		final GridData gd_Label = new GridData();
		gd_Label.minimumWidth = 50;
		outLabel.setLayoutData(gd_Label);
		outLabel.setText(resourceString + ":");
		output = new Text(composite, SWT.BORDER);
		GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gd.minimumWidth = 50;
		gd.widthHint = 256;
		output.setLayoutData(gd);
		output.setText(prefs.getOutputDir());
		output.addListener(SWT.Modify, nameModifyListener);
		// TODO use resource!
		uz = new Button(composite, SWT.CHECK);
		this.uz.setToolTipText("place at end of code:path");
		this.uz.setText("place last in path");
		this.uz.setLayoutData(new GridData());
		uz.setSelection(prefs.getUsePathZ());
		uz.addListener(SWT.Modify, nameModifyListener);

		Label l1 = new Label(composite, SWT.NONE);
		l1.setText("Source directories:");
		String resourceString2 = ErlideUIPlugin
				.getResourceString("wizards.labels.source");
		l1.setText(resourceString2 + ":");
		source = new Text(composite, SWT.BORDER);
		this.source.setToolTipText("enter a list of folders");
		gd = new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1);
		source.setLayoutData(gd);
		source.setText(prefs.getSourceDirsString());
		source.addListener(SWT.Modify, nameModifyListener);

		String resourceString3 = ErlideUIPlugin
				.getResourceString("wizards.labels.include");
		final Label includesLabel = new Label(composite, SWT.NONE);
		includesLabel.setText("Include directories:");
		includesLabel.setText(resourceString3 + ":");
		include = new Text(composite, SWT.BORDER);
		this.include.setToolTipText("enter a list of folders");
		gd = new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1);
		include.setLayoutData(gd);
		include.setText(prefs.getIncludeDirsString());
		include.addListener(SWT.Modify, nameModifyListener);

		String[] backends = RuntimeInfoManager.getDefault().getRuntimeNames()
				.toArray(new String[] {});
		int db = Arrays.binarySearch(backends, RuntimeInfoManager.getDefault()
				.getDefaultRuntime().getName());

		final Label nodeNameLabel_1 = new Label(composite, SWT.NONE);
		nodeNameLabel_1.setText("Build backend:");

		buildBackendName = new Combo(composite, SWT.READ_ONLY);
		final GridData gd_buildBackendName = new GridData(SWT.FILL, SWT.CENTER,
				true, false);
		buildBackendName.setLayoutData(gd_buildBackendName);
		buildBackendName.setItems(backends);
		new Label(composite, SWT.NONE);

		setValid(testPageComplete());

		return composite;
	}

	protected void handleExternalModulesBrowseSelected() {
		String last = externalIncludes.getText();
		// if (last.length() == 0) {
		// last =
		//DebugUIPlugin.getDefault().getDialogSettings().get(LAST_PATH_SETTING);
		// }
		if (last == null) {
			last = ""; //$NON-NLS-1$
		} else {
			last = last.trim();
		}
		FileDialog dialog = new FileDialog(getShell(), SWT.SINGLE);
		dialog.setText("Select file with external modules");
		dialog.setFileName(last);
		dialog.setFilterExtensions(new String[] { "*.erlidex" });
		String result = dialog.open();
		if (result == null) {
			return;
		}
		externalIncludes.setText(result);
	}

	@Override
	protected void performDefaults() {
		// Populate the owner text field with the default value
	}

	@Override
	public boolean performOk() {
		// store the value in the owner text field
		prefs.setOutputDir(output.getText());
		prefs.setUsePathZ(uz.getSelection());
		prefs.setSourceDirsString(source.getText());
		prefs.setIncludeDirsString(include.getText());
		// prefs.setExternalModules(externalIncludes.getText());
		prefs.setRuntimeName(buildBackendName.getText());
		prefs.store();
		return true;
	}

	private final Listener nameModifyListener = new Listener() {
		public void handleEvent(Event e) {
			setValid(testPageComplete());
		}
	};

	protected boolean testPageComplete() {
		if ((output.getText() == null || output.getText().trim().length() == 0)) {
			setErrorMessage(ErlideUIPlugin
					.getResourceString("wizards.errors.outputrequired"));
			return false;
		}

		if ((source.getText() == null || source.getText().trim().length() == 0)) {
			setErrorMessage(ErlideUIPlugin
					.getResourceString("wizards.errors.sourcerequired"));
			return false;
		}

		setErrorMessage(null);
		setMessage(null);
		return true;
	}

	public void propertyChange(PropertyChangeEvent event) {
		ErlLogger.debug("*+> " + event);
	}

}

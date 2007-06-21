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

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.PathEditor;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.PropertyPage;
import org.erlide.basiccore.ErlLogger;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.properties.internal.MockupPreferenceStore;

public class ErlProjectPropertyPage extends PropertyPage implements
		IPropertyChangeListener {

	private ErlangProjectProperties prefs;

	private Text output;

	private Text source;

	private Text include;

	private MockupPreferenceStore mockPrefs;

	private PathEditor fextinc;

	/**
	 * Constructor for ErlProjectPropertyPage.
	 */
	public ErlProjectPropertyPage() {
		super();
	}

	/**
	 * @see PreferencePage#createContents(Composite)
	 */
	@Override
	protected Control createContents(Composite parent) {
		final Object prj = getElement();
		prefs = new ErlangProjectProperties((IProject) prj);
		mockPrefs = new MockupPreferenceStore();
		mockPrefs.addPropertyChangeListener(this);

		// create the composite to hold the widgets
		final Composite composite = new Composite(parent, SWT.NONE);

		// create the desired layout for this wizard page
		final GridLayout gl = new GridLayout();
		gl.numColumns = 2;
		composite.setLayout(gl);

		// create the widgets and their grid data objects
		new Label(composite, SWT.NONE).setText(ErlideUIPlugin
				.getResourceString("wizards.labels.buildoutput") +
				":");
		output = new Text(composite, SWT.BORDER);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		output.setLayoutData(gd);
		output.setText(prefs.getOutputDir());
		output.addListener(SWT.Modify, nameModifyListener);

		Label l1 = new Label(composite, SWT.NONE);
		l1.setText(ErlideUIPlugin.getResourceString("wizards.labels.source") +
				":");
		source = new Text(composite, SWT.BORDER);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		source.setLayoutData(gd);
		source.setText(prefs.getSourceDirsString());
		source.addListener(SWT.Modify, nameModifyListener);

		new Label(composite, SWT.NONE).setText(ErlideUIPlugin
				.getResourceString("wizards.labels.include") +
				":");
		include = new Text(composite, SWT.BORDER);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		include.setLayoutData(gd);
		include.setText(prefs.getIncludeDirsString());
		include.addListener(SWT.Modify, nameModifyListener);

		fextinc = new PathEditor("ext include", "ext inc", "New", composite);
		fextinc.setPreferenceStore(mockPrefs);

		setValid(testPageComplete());

		return composite;
	}

	@Override
	protected void performDefaults() {
		// Populate the owner text field with the default value
	}

	@Override
	public boolean performOk() {
		// store the value in the owner text field
		prefs.setOutputDir(output.getText());
		prefs.setSourceDirsString(source.getText());
		prefs.setIncludeDirsString(include.getText());

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
		ErlLogger.log("*+> " + event);
	}

}

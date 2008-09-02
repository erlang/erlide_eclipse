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
import org.eclipse.core.runtime.IAdaptable;
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
import org.eclipse.swt.widgets.Group;
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

	private Text cookie;
	private Text nodeName;
	Combo runtimeName;
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
		output.addListener(SWT.Modify, modifyListener);
		// TODO use resource!
		uz = new Button(composite, SWT.CHECK);
		this.uz.setToolTipText("place at end of code:path");
		this.uz.setText("place last in path");
		this.uz.setLayoutData(new GridData());
		uz.addListener(SWT.Modify, modifyListener);

		Label l1 = new Label(composite, SWT.NONE);
		l1.setText("Source directories:");
		String resourceString2 = ErlideUIPlugin
				.getResourceString("wizards.labels.source");
		l1.setText(resourceString2 + ":");
		source = new Text(composite, SWT.BORDER);
		this.source.setToolTipText("enter a list of folders");
		gd = new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1);
		source.setLayoutData(gd);
		source.addListener(SWT.Modify, modifyListener);

		String resourceString3 = ErlideUIPlugin
				.getResourceString("wizards.labels.include");
		final Label includesLabel = new Label(composite, SWT.NONE);
		includesLabel.setText("Include directories:");
		includesLabel.setText(resourceString3 + ":");
		include = new Text(composite, SWT.BORDER);
		this.include.setToolTipText("enter a list of folders");
		gd = new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1);
		include.setLayoutData(gd);
		include.addListener(SWT.Modify, modifyListener);

		final Label nodeNameLabel_1 = new Label(composite, SWT.NONE);
		nodeNameLabel_1.setText("Build backend:");

		final Group group = new Group(composite, SWT.NONE);
		final GridData gd_group = new GridData(SWT.FILL, SWT.FILL, false, false);
		gd_group.heightHint = 84;
		group.setLayoutData(gd_group);
		final GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 2;
		group.setLayout(gridLayout);

		final Label runtimeLabel = new Label(group, SWT.NONE);
		runtimeLabel.setText("Runtime");

		runtimeName = new Combo(group, SWT.READ_ONLY);
		runtimeName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false,
				false));
		runtimeName.addListener(SWT.Modify, modifyListener);

		final Label nodeNameLabel = new Label(group, SWT.NONE);
		nodeNameLabel.setText("Node name");

		nodeName = new Text(group, SWT.BORDER);
		nodeName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		nodeName.addListener(SWT.Modify, modifyListener);

		final Label cookieLabel = new Label(group, SWT.NONE);
		cookieLabel.setText("Cookie");

		cookie = new Text(group, SWT.BORDER);
		final GridData gd_cookie = new GridData(SWT.FILL, SWT.CENTER, true,
				false);
		cookie.setLayoutData(gd_cookie);

		new Label(composite, SWT.NONE);

		performDefaults();
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
		final IAdaptable prj = getElement();
		ErlangProjectProperties prefs = new ErlangProjectProperties(
				(IProject) prj.getAdapter(IProject.class));

		uz.setSelection(prefs.getUsePathZ());
		source.setText(prefs.getSourceDirsString());
		include.setText(prefs.getIncludeDirsString());
		output.setText(prefs.getOutputDir());

		String[] runtimes = RuntimeInfoManager.getDefault().getRuntimeNames()
				.toArray(new String[] {});
		int db = Arrays.binarySearch(runtimes, RuntimeInfoManager.getDefault()
				.getDefaultRuntime().getName());
		runtimeName.setItems(runtimes);
		runtimeName.select(db);

		nodeName.setText(prefs.getNodeName());
		cookie.setText(prefs.getCookie());

	}

	@Override
	public boolean performOk() {
		// store the value in the owner text field
		final IAdaptable prj = getElement();
		ErlangProjectProperties prefs = new ErlangProjectProperties(
				(IProject) prj.getAdapter(IProject.class));

		prefs.setOutputDir(output.getText());
		prefs.setUsePathZ(uz.getSelection());
		prefs.setSourceDirsString(source.getText());
		prefs.setIncludeDirsString(include.getText());
		prefs.setRuntimeName(runtimeName.getText());
		prefs.setNodeName(nodeName.getText());
		prefs.setCookie(cookie.getText());
		prefs.store();
		return true;
	}

	private final Listener modifyListener = new Listener() {
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
		if (runtimeName.getText() == null
				|| runtimeName.getText().trim().length() == 0) {
			setErrorMessage("The backend's runtime has to be specified");
			return false;
		}
		if (nodeName.getText() == null
				|| nodeName.getText().trim().length() == 0) {
			setErrorMessage("The backend's node name has to be specified");
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

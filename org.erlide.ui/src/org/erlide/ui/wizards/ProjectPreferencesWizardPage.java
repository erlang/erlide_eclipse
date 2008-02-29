/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.wizards;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.ui.ErlideUIPlugin;

/**
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com]
 */
public class ProjectPreferencesWizardPage extends WizardPage {

	private Text output;
	private Text source;
	private Text include;
	private Text backendCookie;
	private Text backendName;
	private Button uz;
	private Text externalModules;
	private Button externalModulesBrowse;

	ErlangProjectProperties prefs;

	/**
	 * Constructor inherited from parent
	 * 
	 * @param pageName
	 */
	public ProjectPreferencesWizardPage(String pageName) {
		super(pageName);
	}

	/**
	 * Constructor inherited from parents parent
	 * 
	 * @param pageName
	 * @param title
	 * @param titleImage
	 */
	public ProjectPreferencesWizardPage(String pageName, String title,
			ImageDescriptor titleImage) {
		super(pageName, title, titleImage);
	}

	/**
	 * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
	 */
	public void createControl(Composite parent) {
		prefs = new ErlangProjectProperties();

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
		outLabel.setText("output Dir");
		final GridData gd_Label = new GridData();
		gd_Label.minimumWidth = 50;
		outLabel.setLayoutData(gd_Label);
		outLabel.setText(resourceString + ":");
		output = new Text(composite, SWT.BORDER);
		GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gd.minimumWidth = 50;
		gd.widthHint = 384;
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
		l1.setText("sources");
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
		includesLabel.setText("includes");
		includesLabel.setText(resourceString3 + ":");
		include = new Text(composite, SWT.BORDER);
		this.include.setToolTipText("enter a list of folders");
		gd = new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1);
		include.setLayoutData(gd);
		include.setText(prefs.getIncludeDirsString());
		include.addListener(SWT.Modify, nameModifyListener);

		createExternalModuleEditor(composite);

		final Label nodeNameLabel = new Label(composite, SWT.NONE);
		nodeNameLabel.setText("Node name");

		backendName = new Text(composite, SWT.BORDER);
		final GridData gd_backendName = new GridData(SWT.FILL, SWT.CENTER,
				true, false);
		backendName.setLayoutData(gd_backendName);
		backendName.setText(prefs.getBackendName());
		new Label(composite, SWT.NONE);

		final Label nodeCookieLabel = new Label(composite, SWT.NONE);
		nodeCookieLabel.setText("Node cookie");

		backendCookie = new Text(composite, SWT.BORDER);
		final GridData gd_backendCookie = new GridData(SWT.FILL, SWT.CENTER,
				true, false);
		backendCookie.setLayoutData(gd_backendCookie);
		backendCookie.setText(prefs.getBackendCookie());
		new Label(composite, SWT.NONE);

		final Button discoverBtn = new Button(composite, SWT.PUSH);
		discoverBtn.setText("Discover paths...");
		discoverBtn.addListener(SWT.Selection, new Listener() {

			public void handleEvent(Event event) {
				discoverPaths();
			}
		});

		// set the composite as the control for this page
		setControl(composite);
	}

	protected void discoverPaths() {
		final WizardNewProjectCreationPage prev = (WizardNewProjectCreationPage) getPreviousPage();
		final IPath loc = prev.getLocationPath();
		final File dir = loc.toFile();

		if (dir.exists()) {
			final List<String> src = search("erl", dir, new ArrayList<String>(
					10));
			final String[] srcs = dirs(src, loc);

			final List<String> inc = search("hrl", dir, new ArrayList<String>(
					10));
			final String[] incs = dirs(inc, loc);

			source.setText(ErlangProjectProperties.pack(srcs));
			include.setText(ErlangProjectProperties.pack(incs));
		}
	}

	private String[] dirs(List<String> list, IPath ref) {
		final int n = ref.segmentCount();
		final List<String> res = new ArrayList<String>(10);
		for (final Iterator<String> iter = list.iterator(); iter.hasNext();) {
			final String element = iter.next();
			IPath p = new Path(element);
			p = p.removeLastSegments(1).removeFirstSegments(n).setDevice(null);
			String ps = p.toString();
			if ("".equals(ps)) {
				ps = ".";
			}
			if (res.indexOf(ps) < 0) {
				res.add(ps);
			}
		}
		return res.toArray(new String[res.size()]);
	}

	private List<String> search(String ext, File file, List<String> list) {

		if (file.isFile()) {
			final IPath path = new Path(file.getPath());
			if (path.getFileExtension() != null
					&& path.getFileExtension().equals(ext)) {
				list.add(file.getPath());
			}
		} else if (file.isDirectory()) {
			final File[] fs = file.listFiles();
			for (final File f : fs) {
				search(ext, f, list);
			}
		}
		return list;
	}

	protected boolean testPageComplete() {
		if (null != output
				&& (output.getText() == null || output.getText().trim()
						.length() == 0)) {
			setErrorMessage(ErlideUIPlugin
					.getResourceString("wizards.errors.outputrequired"));
			return false;
		}

		if (null != source
				&& (source.getText() == null || source.getText().trim()
						.length() == 0)) {
			setErrorMessage(ErlideUIPlugin
					.getResourceString("wizards.errors.sourcerequired"));
			return false;
		}

		setErrorMessage(null);
		setMessage(null);
		return true;
	}

	private final Listener nameModifyListener = new Listener() {

		public void handleEvent(Event e) {
			prefs.setOutputDir(output.getText());
			prefs.setSourceDirsString(source.getText());
			prefs.setIncludeDirsString(include.getText());

			setPageComplete(testPageComplete());
		}
	};

	public ErlangProjectProperties getPrefs() {
		return prefs;
	}

	private void createExternalModuleEditor(final Composite parent) {
		Composite composite = parent;
		// Composite composite = new Composite(parent, SWT.NONE);
		// GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		// composite.setLayoutData(gd);
		// GridLayout layout = new GridLayout(2, false);
		// composite.setLayout(layout);

		String resourceString4 = "External Modules File";
		new Label(composite, SWT.NONE).setText(resourceString4 + ":");
		externalModules = new Text(composite, SWT.BORDER);
		externalModules.setToolTipText("enter a list of folders");
		// gd = new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1);
		// externalModules.setLayoutData(gd);
		externalModules.setText(prefs.getExternalModules());
		GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gd.minimumWidth = 50;
		gd.widthHint = 384;
		externalModules.setLayoutData(gd);
		externalModules.addListener(SWT.Modify, nameModifyListener);
		externalModulesBrowse = new Button(composite, SWT.BORDER);
		externalModulesBrowse.setText("Browse...");
		// externalModulesBrowse.setLayoutData(new GridData());
		// externalModulesBrowse.setFont(font);
		externalModulesBrowse.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				handleExternalModulesBrowseSelected();
			}

		});
	}

	protected void handleExternalModulesBrowseSelected() {
		String last = externalModules.getText();
		// if (last.length() == 0) {
		// last =
		// DebugUIPlugin.getDefault().getDialogSettings().get(LAST_PATH_SETTING);
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
		externalModules.setText(result);
	}

}

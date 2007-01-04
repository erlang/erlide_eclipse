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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
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

	Text output;

	Text source;

	Text include;

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
		gl.numColumns = 2;
		composite.setLayout(gl);

		// create the widgets and their grid data objects
		new Label(composite, SWT.NONE).setText(ErlideUIPlugin
				.getResourceString("wizards.labels.buildoutput")
				+ ":");
		output = new Text(composite, SWT.BORDER);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		output.setLayoutData(gd);
		output.setText(prefs.getOutputDir());
		output.addListener(SWT.Modify, nameModifyListener);

		new Label(composite, SWT.NONE).setText(ErlideUIPlugin
				.getResourceString("wizards.labels.source")
				+ ":");
		source = new Text(composite, SWT.BORDER);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		source.setLayoutData(gd);
		source.setText(prefs.getSourceDirsString());
		source.addListener(SWT.Modify, nameModifyListener);

		new Label(composite, SWT.NONE).setText(ErlideUIPlugin
				.getResourceString("wizards.labels.include")
				+ ":");
		include = new Text(composite, SWT.BORDER);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		include.setLayoutData(gd);
		include.setText(prefs.getIncludeDirsString());
		include.addListener(SWT.Modify, nameModifyListener);

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

	private String[] dirs(List list, IPath ref) {
		final int n = ref.segmentCount();
		final List<String> res = new ArrayList<String>(10);
		for (final Iterator iter = list.iterator(); iter.hasNext();) {
			final String element = (String) iter.next();
			IPath p = new Path(element);
			p = p.removeLastSegments(1).removeFirstSegments(n).setDevice(null);
			String ps = p.toString();
			if (ps.equals("")) {
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

}

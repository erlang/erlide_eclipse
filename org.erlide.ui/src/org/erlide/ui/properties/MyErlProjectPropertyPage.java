/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.PathEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.PropertyPage;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.ui.properties.internal.MockupPreferenceStore;

public class MyErlProjectPropertyPage extends PropertyPage implements
		IPropertyChangeListener {

	private Text text;
	private TabFolder tabFolder;
	private ErlangProjectProperties prefs;
	private MockupPreferenceStore mockPrefs;
	private PathEditor fextinc;
	private PathEditor fSourceEditor;
	private PathEditor fIncludeEditor;
	private PathEditor fExternalIncludeEditor;

	public MyErlProjectPropertyPage() {
		super();
	}

	@Override
	protected Control createContents(Composite parent) {
		final IProject prj = (IProject) getElement();
		prefs = new ErlangProjectProperties(prj);
		mockPrefs = new MockupPreferenceStore();
		mockPrefs.addPropertyChangeListener(this);

		// create the composite to hold the widgets
		final Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new FillLayout());

		// fextinc = new PathEditor("ext include", "ext inc", "New",
		// this.composite_1);

		this.tabFolder = new TabFolder(composite, SWT.NONE);

		final TabItem sourceTab = new TabItem(this.tabFolder, SWT.NONE);
		sourceTab.setText("Source");

		final TabItem t2 = new TabItem(this.tabFolder, SWT.NONE);
		t2.setText("Include");

		final Composite includeComposite = new Composite(this.tabFolder,
				SWT.NONE);
		includeComposite.setBounds(0, 0, 443, 305);
		includeComposite.setLayout(new GridLayout());
		t2.setControl(includeComposite);
		final Composite composite_5 = new Composite(includeComposite, SWT.NONE);
		final GridData gd_composite_5 = new GridData(SWT.FILL, SWT.CENTER,
				true, false);
		composite_5.setLayoutData(gd_composite_5);
		composite_5.setLayout(new GridLayout());

		final Composite composite_3 = new Composite(includeComposite, SWT.NONE);
		final GridData gd_composite_3 = new GridData(SWT.FILL, SWT.CENTER,
				true, false);
		composite_3.setLayoutData(gd_composite_3);
		composite_3.setLayout(new GridLayout());

		fExternalIncludeEditor = new PathEditor("ext include",
				"External include directories:", "New", composite_3);
		fIncludeEditor = new PathEditor("ext include",
				"Project include directories:", "New", composite_5);

		final TabItem t3 = new TabItem(this.tabFolder, SWT.NONE);
		t3.setText("Projects");

		final Composite composite_1 = new Composite(this.tabFolder, SWT.NONE);
		composite_1.setLayout(new GridLayout());
		t3.setControl(composite_1);

		final Composite sourceComposite = new Composite(this.tabFolder,
				SWT.NONE);
		sourceComposite.setBounds(0, 0, 443, 305);
		final GridLayout gridLayout = new GridLayout();

		sourceComposite.setLayout(gridLayout);
		sourceTab.setControl(sourceComposite);

		final Composite composite_2 = new Composite(sourceComposite, SWT.NONE);
		final GridData gd_composite_2 = new GridData(SWT.FILL, SWT.CENTER,
				true, false);
		composite_2.setLayoutData(gd_composite_2);
		composite_2.setLayout(new GridLayout());

		fSourceEditor = new PathEditor("ext include",
				"Source directories for this project:", "New", composite_2);

		this.text = new Text(sourceComposite, SWT.BORDER);
		this.text
				.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final TabItem orderTab = new TabItem(this.tabFolder, SWT.NONE);
		orderTab.setText("Order");

		final Composite composite_4 = new Composite(this.tabFolder, SWT.NONE);
		composite_4.setLayout(new GridLayout());
		orderTab.setControl(composite_4);

		final Button usePathzForButton = new Button(composite_4, SWT.CHECK);
		usePathzForButton.setText("use pathz for the project");
		return composite;
	}

	public void propertyChange(PropertyChangeEvent event) {
		// TODO Auto-generated method stub

	}

}

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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PathEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.ui.dialogs.PropertyPage;
import org.eclipse.ui.preferences.ScopedPreferenceStore;
import org.erlide.backend.RuntimeInfo;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.preferences.ProjectPreferencesConstants;

/*TODO ...this should implement IworkspacePreferencePage if it's going to be 
 * used as "default erlang project" page...*/
public class ErlProjectPropertyPage extends PropertyPage {

	public ErlProjectPropertyPage() {
		super();
	}

	@Override
	protected IPreferenceStore doGetPreferenceStore() {
		final IPreferenceStore store = new ScopedPreferenceStore(
				new ProjectScope(getProject()), ErlangPlugin.PLUGIN_ID);
		return store;
	}

	private TabFolder tabFolder;
	private final List<FieldEditor> editors = new ArrayList<FieldEditor>();

	@Override
	protected Control createContents(final Composite aparent) {
		final Composite parent = new Composite(aparent, SWT.NONE);
		final GridLayout layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		parent.setLayout(layout);
		parent.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		parent.setLayout(new FillLayout());

		final Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new FillLayout());
		this.tabFolder = new TabFolder(composite, SWT.NONE);

		// /////////////////////////////////////
		final TabItem sourceTab = new TabItem(this.tabFolder, SWT.NONE);
		sourceTab.setText("Source");

		final Composite sourceComposite = new Composite(this.tabFolder,
				SWT.NONE);
		sourceComposite.setBounds(0, 0, 443, 305);
		final GridLayout gridLayout = new GridLayout();
		sourceComposite.setLayout(gridLayout);
		sourceTab.setControl(sourceComposite);

		final String root = getProject().getLocation().toString();
		editors.add(new ProjectPathEditor(
				ProjectPreferencesConstants.SOURCE_DIRS,
				"Source directories for this project:", "New", root,
				createComposite(sourceComposite)));

		editors.add(new DirectoryFieldEditor(
				ProjectPreferencesConstants.OUTPUT_DIR, "Output directory:",
				createComposite(sourceComposite)));

		// //////////////////////

		final TabItem t2 = new TabItem(this.tabFolder, SWT.NONE);
		t2.setText("Include");

		final Composite includeComposite = new Composite(this.tabFolder,
				SWT.NONE);
		includeComposite.setBounds(0, 0, 443, 305);
		includeComposite.setLayout(new GridLayout());
		t2.setControl(includeComposite);

		editors.add(new PathEditor(ProjectPreferencesConstants.INCLUDE_DIRS,
				"Project include directories:", "New",
				createComposite(includeComposite)));

		editors.add(new PathEditor(
				ProjectPreferencesConstants.EXTERNAL_INCLUDES,
				"External include directories:", "New",
				createComposite(includeComposite)));

		// ////////////////////////

		final TabItem t3 = new TabItem(this.tabFolder, SWT.NONE);
		t3.setText("Dependencies");

		final Composite dependenciesComposite = new Composite(this.tabFolder,
				SWT.NONE);
		dependenciesComposite.setLayout(new GridLayout());
		t3.setControl(dependenciesComposite);

		// //////////////////

		final TabItem buildTab = new TabItem(this.tabFolder, SWT.NONE);
		buildTab.setText("Building");

		final Composite backendComposite = new Composite(this.tabFolder,
				SWT.NONE);
		backendComposite.setLayout(new GridLayout());
		buildTab.setControl(backendComposite);

		final Collection<RuntimeInfo> rs = ErlangCore.getRuntimeInfoManager()
				.getRuntimes();
		final List<String[]> vv = new ArrayList<String[]>();
		for (final RuntimeInfo ri : rs) {
			vv.add(new String[] { ri.getName(), ri.getName() });
		}
		final String[][] values = vv.toArray(new String[][] {});

		final Composite rtComposite = createComposite(backendComposite);
		editors.add(new ComboFieldEditor("runtimes", "Installations", values,
				rtComposite));

		editors.add(new StringFieldEditor(
				ProjectPreferencesConstants.RUNTIME_NAME, "Node name",
				rtComposite));

		// //////////////////////////////////////////////

		final TabItem codepathTabItem = new TabItem(tabFolder, SWT.NONE);
		codepathTabItem.setText("Codepath");

		final Composite codepathComposite = new Composite(tabFolder, SWT.NONE);
		codepathComposite.setLayout(new GridLayout());
		codepathTabItem.setControl(codepathComposite);

		editors.add(new CodePathEditor("codepath",
				"Order the code:path of the backend",
				createComposite(codepathComposite)));

		// ///////////////////////////////////////////

		initFieldEditors();

		return parent;
	}

	private IProject getProject() {
		return (IProject) getElement().getAdapter(IProject.class);
	}

	private Composite createComposite(final Composite parent) {
		final Composite result = new Composite(parent, SWT.NONE);
		final GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
		result.setLayoutData(gd);
		return result;
	}

	private void initFieldEditors() {
		for (final FieldEditor editor : editors) {
			editor.setPage(this);
			editor.setPreferenceStore(getPreferenceStore());
			editor.load();
		}
	}

	@Override
	protected void performDefaults() {
		for (final FieldEditor editor : editors) {
			editor.loadDefault();
		}
		super.performDefaults();
	}

	@Override
	public boolean performOk() {
		for (final FieldEditor editor : editors) {
			editor.store();
		}
		return super.performOk();
	}

}

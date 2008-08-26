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
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PathEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.ui.IWorkbench;
import org.erlide.runtime.ErlLogger;
import org.erlide.ui.ErlideUIPlugin;

import com.bdaum.overlayPages.FieldEditorOverlayPage;
import com.bdaum.overlayPages.OverlayPage;

public class ErlProjectPropertyPage extends OverlayPage implements
		IPreferenceChangeListener, IPropertyChangeListener {

	public ErlProjectPropertyPage() {
		super();
	}

	@Override
	protected IPreferenceStore doGetPreferenceStore() {
		final IPreferenceStore store = ErlideUIPlugin.getDefault()
				.getPreferenceStore();
		store.addPropertyChangeListener(this);
		return store;
	}

	private TabFolder tabFolder;
	private List<FieldEditor> editors = new ArrayList<FieldEditor>();

	@Override
	protected Control createContents(Composite aparent) {
		Composite parent = (Composite) super.createContents(aparent);
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

		editors.add(new PathEditor("sources",
				"Source directories for this project:", "New",
				createComposite(sourceComposite)));

		editors.add(new DirectoryFieldEditor("output", "Output directory:",
				createComposite(sourceComposite)));

		// //////////////////////

		final TabItem t2 = new TabItem(this.tabFolder, SWT.NONE);
		t2.setText("Include");

		final Composite includeComposite = new Composite(this.tabFolder,
				SWT.NONE);
		includeComposite.setBounds(0, 0, 443, 305);
		includeComposite.setLayout(new GridLayout());
		t2.setControl(includeComposite);

		editors.add(new PathEditor("ext include",
				"Project include directories:", "New",
				createComposite(includeComposite)));

		editors.add(new PathEditor("ext include",
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

		final TabItem backendTab = new TabItem(this.tabFolder, SWT.NONE);
		backendTab.setText("Backend");

		final Composite backendComposite = new Composite(this.tabFolder,
				SWT.NONE);
		backendComposite.setLayout(new GridLayout());
		backendTab.setControl(backendComposite);

		String[][] values = new String[][] { { "a", "a" }, { "b", "b" } };

		final Composite rtComposite = createComposite(backendComposite);
		editors.add(new ComboFieldEditor("runtimes", "Installations", values,
				rtComposite));

		editors.add(new StringFieldEditor("backendName", "Node name",
				rtComposite));

		editors.add(new StringFieldEditor("backendCookie", "Cookie",
				rtComposite));

		editors.add(new StringFieldEditor("extraArgs", "Extra arguments",
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

		for (FieldEditor editor : editors) {
			editor.setPage(this);
			editor.setPreferenceStore(getPreferenceStore());
			editor.load();
		}

		return parent;
	}

	private Composite createComposite(final Composite parent) {
		final Composite result = new Composite(parent, SWT.NONE);
		final GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
		result.setLayoutData(gd);
		return result;
	}

	@Override
	protected String getPageId() {
		return "org.erlide.ui.properties.myErlangProjectPropertyPage";
	}

	public void init(IWorkbench workbench) {
		setDescription("These values will be used as defaults for newly created Erlang projects.");
	}

	public static String getOverlayedPreferenceValue(IPreferenceStore store,
			IResource resource, String pageId, String key) {
		IProject project = resource.getProject();
		String value = null;
		if (useProjectSettings(project, pageId)) {
			value = getProperty(resource, pageId, key);
		}
		if (value != null) {
			return value;
		}
		return store.getString(key);
	}

	private static boolean useProjectSettings(IResource resource, String pageId) {
		String use = getProperty(resource, pageId,
				FieldEditorOverlayPage.USEPROJECTSETTINGS);
		return "true".equals(use);
	}

	private static String getProperty(IResource resource, String pageId,
			String key) {
		try {
			return resource
					.getPersistentProperty(new QualifiedName(pageId, key));
		} catch (CoreException e) {
		}
		return null;
	}

	@Override
	protected void performDefaults() {
		for (FieldEditor editor : editors) {
			editor.loadDefault();
		}
		super.performDefaults();
	}

	@Override
	public boolean performOk() {
		for (FieldEditor editor : editors) {
			editor.store();
		}
		return super.performOk();
	}

	@Override
	public IAdaptable getElement() {
		final IProject prj = (IProject) super.getElement().getAdapter(
				IProject.class);
		return prj;
	}

	public void preferenceChange(PreferenceChangeEvent event) {
		ErlLogger.debug("## change %s %s: %s -> %s", event.getNode(), event
				.getKey(), event.getOldValue(), event.getNewValue());
	}

	public void propertyChange(PropertyChangeEvent event) {
		ErlLogger.debug("#@ change %s: %s -> %s", event.getProperty(), event
				.getOldValue(), event.getNewValue());
	}
}

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
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
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
import org.erlide.basiccore.ErlLogger;
import org.erlide.ui.ErlideUIPlugin;

import com.bdaum.overlayPages.FieldEditorOverlayPage;
import com.bdaum.overlayPages.OverlayPage;

public class MyErlProjectPropertyPage extends OverlayPage implements
		IPreferenceChangeListener, IPropertyChangeListener {

	public MyErlProjectPropertyPage() {
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

	// private ErlangProjectProperties prefs;

	@Override
	protected Control createContents(final Composite aparent) {
		final Composite parent = (Composite) super.createContents(aparent);
		parent.setLayout(new FillLayout());

		final Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new FillLayout());
		tabFolder = new TabFolder(composite, SWT.NONE);

		// /////////////////////////////////////
		final TabItem sourceTab = new TabItem(tabFolder, SWT.NONE);
		sourceTab.setText("Source");

		final Composite sourceComposite = new Composite(tabFolder, SWT.NONE);
		sourceComposite.setBounds(0, 0, 443, 305);
		final GridLayout gridLayout = new GridLayout();
		sourceComposite.setLayout(gridLayout);
		sourceTab.setControl(sourceComposite);

		new PathEditor("sources", "Source directories for this project:",
				"New", createComposite(sourceComposite));

		new DirectoryFieldEditor("output", "Output directory:",
				createComposite(sourceComposite));

		// //////////////////////

		final TabItem t2 = new TabItem(tabFolder, SWT.NONE);
		t2.setText("Include");

		final Composite includeComposite = new Composite(tabFolder, SWT.NONE);
		includeComposite.setBounds(0, 0, 443, 305);
		includeComposite.setLayout(new GridLayout());
		t2.setControl(includeComposite);

		new PathEditor("ext include", "Project include directories:", "New",
				createComposite(includeComposite));

		new PathEditor("ext include", "External include directories:", "New",
				createComposite(includeComposite));

		// ////////////////////////

		final TabItem t3 = new TabItem(tabFolder, SWT.NONE);
		t3.setText("Dependencies");

		final Composite dependenciesComposite = new Composite(tabFolder,
				SWT.NONE);
		dependenciesComposite.setLayout(new GridLayout());
		t3.setControl(dependenciesComposite);

		// //////////////////

		final TabItem backendTab = new TabItem(tabFolder, SWT.NONE);
		backendTab.setText("Backend");

		final Composite backendComposite = new Composite(tabFolder, SWT.NONE);
		backendComposite.setLayout(new GridLayout());
		backendTab.setControl(backendComposite);

		final String[][] values = new String[][] { { "a", "a" }, { "b", "b" } };

		final Composite rtComposite = createComposite(backendComposite);
		new ComboFieldEditor("runtimes", "Runtimes", values, rtComposite);

		new StringFieldEditor("backendName", "Node name", rtComposite);

		new StringFieldEditor("backendCookie", "Cookie", rtComposite);

		new StringFieldEditor("extraArgs", "Extra arguments", rtComposite);

		// //////////////////////////////////////////////

		final TabItem codepathTabItem = new TabItem(tabFolder, SWT.NONE);
		codepathTabItem.setText("Codepath");

		final Composite codepathComposite = new Composite(tabFolder, SWT.NONE);
		codepathComposite.setLayout(new GridLayout());
		codepathTabItem.setControl(codepathComposite);

		new CodePathEditor("codepath", "Order the code:path of the backend",
				createComposite(codepathComposite));

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

	public void init(final IWorkbench workbench) {
		setDescription("These values will be used as defaults for newly created Erlang projects.");
	}

	public static String getOverlayedPreferenceValue(
			final IPreferenceStore store, final IResource resource,
			final String pageId, final String key) {
		final IProject project = resource.getProject();
		String value = null;
		if (useProjectSettings(project, pageId)) {
			value = getProperty(resource, pageId, key);
		}
		if (value != null) {
			return value;
		}
		return store.getString(key);
	}

	private static boolean useProjectSettings(final IResource resource,
			final String pageId) {
		final String use = getProperty(resource, pageId,
				FieldEditorOverlayPage.USEPROJECTSETTINGS);
		return "true".equals(use);
	}

	private static String getProperty(final IResource resource,
			final String pageId, final String key) {
		try {
			return resource
					.getPersistentProperty(new QualifiedName(pageId, key));
		} catch (final CoreException e) {
		}
		return null;
	}

	@Override
	public boolean performOk() {
		return super.performOk();
	}

	@Override
	public IAdaptable getElement() {
		final IProject prj = (IProject) super.getElement();
		// prefs = new ErlangProjectProperties(prj);
		return prj;
	}

	public void preferenceChange(final PreferenceChangeEvent event) {
		ErlLogger.debug("## change %s %s: %s -> %s", event.getNode(), event
				.getKey(), event.getOldValue(), event.getNewValue());
	}

	public void propertyChange(final PropertyChangeEvent event) {
		ErlLogger.debug("#@ change %s: %s -> %s", event.getProperty(), event
				.getOldValue(), event.getNewValue());
	}
}

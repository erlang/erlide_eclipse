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
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PathEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.preferences.ProjectPreferencesConstants;
import org.erlide.jinterface.backend.RuntimeInfoListener;
import org.erlide.jinterface.util.ErlLogger;

import com.bdaum.overlayPages.FieldEditorOverlayPage;
import com.bdaum.overlayPages.PropertyStore;

public class OldErlProjectPropertyPage extends FieldEditorOverlayPage implements
		IWorkbenchPropertyPage, IPropertyChangeListener, RuntimeInfoListener {

	/**
	 * Constructor for ErlProjectPropertyPage.
	 */
	public OldErlProjectPropertyPage() {
		super("Projet props", SWT.NONE);
		ErlangCore.getRuntimeInfoManager().addListener(this);
	}

	protected void handleBrowseSelected(final Text text,
			final String selectTipString, final String extension) {
		String last = text.getText();
		if (last == null) {
			last = ""; //$NON-NLS-1$
		} else {
			last = last.trim();
		}
		final FileDialog dialog = new FileDialog(getShell(), SWT.SINGLE);
		dialog.setText(selectTipString);
		dialog.setFileName(last);
		dialog.setFilterExtensions(new String[] { extension });
		final String result = dialog.open();
		if (result == null) {
			return;
		}
		text.setText(result);
	}

	@Override
	public void propertyChange(final PropertyChangeEvent event) {
		ErlLogger.debug("*+> " + event);
	}

	/**
	 * The element.
	 */
	private IAdaptable element;

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IWorkbenchPropertyPage#getElement()
	 */
	@Override
	public IAdaptable getElement() {
		return element;
	}

	/**
	 * Sets the element that owns properties shown on this page.
	 * 
	 * @param element
	 *            the element
	 */
	@Override
	public void setElement(IAdaptable element) {
		this.element = element;
		IProject prj = (IProject) element.getAdapter(IProject.class);
		setPreferenceStore(new PropertyStore(prj, super.getPreferenceStore(),
				"id"));
	}

	@Override
	protected void createFieldEditors() {

		IPreferenceStore x = getPreferenceStore();

		IProject prj = (IProject) element.getAdapter(IProject.class);

		addField(new DirectoryFieldEditor(ErlangPlugin.PLUGIN_ID + "/"
				+ ProjectPreferencesConstants.SOURCE_DIRS, "Output directory:",
				getFieldEditorParent()));
		PathEditor src = new ProjectPathEditor("id", "Source directories",
				"Select directory:", getFieldEditorParent());
		((ProjectPathEditor) src).setProject(prj);
		addField(src);
		PathEditor inc = new ProjectPathEditor("id", "Include directories",
				"Select directory:", getFieldEditorParent());
		((ProjectPathEditor) inc).setProject(prj);
		addField(inc);
		PathEditor tst = new ProjectPathEditor("id", "Test source directories",
				"Select directory:", getFieldEditorParent());
		((ProjectPathEditor) tst).setProject(prj);
		addField(tst);
		addField(new ComboFieldEditor("id", "Runtime version", new String[][] {
				{ "name_1", "value_1" }, { "name_2", "value_2" } },
				getFieldEditorParent()));
	}

	public void infoChanged() {
	}

	@Override
	protected String getPageId() {
		return null;
	}

	public void init(IWorkbench workbench) {
	}
}

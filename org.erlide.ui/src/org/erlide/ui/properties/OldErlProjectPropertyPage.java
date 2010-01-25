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
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
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

public class OldErlProjectPropertyPage extends FieldEditorOverlayPage implements
		IWorkbenchPropertyPage, IPropertyChangeListener, RuntimeInfoListener {

	/**
	 * Constructor for ErlProjectPropertyPage.
	 */
	public OldErlProjectPropertyPage() {
		super("Project props", GRID);
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

	@Override
	protected void createFieldEditors() {
		IProject prj = (IProject) getElement().getAdapter(IProject.class);

		addField(new DirectoryFieldEditor(ErlangPlugin.PLUGIN_ID + "/"
				+ ProjectPreferencesConstants.OUTPUT_DIR, "Output directory:",
				getFieldEditorParent()));

		ProjectPathEditor src = new ProjectPathEditor(ErlangPlugin.PLUGIN_ID
				+ "/" + ProjectPreferencesConstants.SOURCE_DIRS,
				"Source directories", "Select directory:",
				getFieldEditorParent());
		src.setProject(prj);
		addField(src);

		ProjectPathEditor inc = new ProjectPathEditor(ErlangPlugin.PLUGIN_ID
				+ "/" + ProjectPreferencesConstants.INCLUDE_DIRS,
				"Include directories", "Select directory:",
				getFieldEditorParent());
		inc.setProject(prj);
		addField(inc);

		ProjectPathEditor tst = new ProjectPathEditor(ErlangPlugin.PLUGIN_ID
				+ "/" + ProjectPreferencesConstants.TEST_DIRS,
				"Test source directories", "Select directory:",
				getFieldEditorParent());
		tst.setProject(prj);
		addField(tst);

		String[][] runtimes = new String[][] { { "name_1", "value_1" },
				{ "name_2", "value_2" } };
		addField(new ComboFieldEditor(ErlangPlugin.PLUGIN_ID + "/"
				+ ProjectPreferencesConstants.RUNTIME_VERSION,
				"Runtime version", runtimes, getFieldEditorParent()));
	}

	public void infoChanged() {
	}

	@Override
	protected String getPageId() {
		return "org.erlide.ui.properties.erlangProjectPropertyPage";
	}

	public void init(IWorkbench workbench) {
	}
}

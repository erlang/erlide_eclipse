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

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.erlide.core.preferences.OldErlangProjectProperties;
import org.erlide.core.preferences.ProjectPreferencesConstants;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.util.PreferencesUtils;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.RuntimeInfoManager;

import com.bdaum.overlayPages.FieldEditorOverlayPage;

public class OldErlProjectPropertyPage extends FieldEditorOverlayPage implements
		IWorkbenchPropertyPage, IPropertyChangeListener {

	/**
	 * Constructor for ErlProjectPropertyPage.
	 */
	public OldErlProjectPropertyPage() {
		super("Erlang project properties", GRID);
		setPropertiesOnly();
	}

	@Override
	public void propertyChange(final PropertyChangeEvent event) {
		ErlLogger.debug("*+> " + event);
	}

	@Override
	protected void createFieldEditors() {
		IProject prj = (IProject) getElement().getAdapter(IProject.class);

		Composite fieldEditorParent = getFieldEditorParent();
		ProjectDirectoryFieldEditor out = new ProjectDirectoryFieldEditor(
				ProjectPreferencesConstants.OUTPUT_DIR, "Output directory:",
				fieldEditorParent, prj);
		addField(out);

		ProjectPathEditor src = new ProjectPathEditor(
				ProjectPreferencesConstants.SOURCE_DIRS, "Source directories:",
				"Select directory:", fieldEditorParent, prj);
		addField(src);

		ProjectPathEditor inc = new ProjectPathEditor(
				ProjectPreferencesConstants.INCLUDE_DIRS,
				"Include directories:", "Select directory:", fieldEditorParent,
				prj);
		addField(inc);

		IPreferenceStore ps = getPreferenceStore();
		OldErlangProjectProperties props = new OldErlangProjectProperties(prj);
		List<String> tstDirs = props.getTestDirs();
		String tstStr = PreferencesUtils.packList(tstDirs);
		ps.setValue(ProjectPreferencesConstants.TEST_DIRS, tstStr);

		ProjectPathEditor tst = new ProjectPathEditor(
				ProjectPreferencesConstants.TEST_DIRS,
				"Test source directories:", "Select directory:",
				fieldEditorParent, prj);
		tst.setEnabled(false, fieldEditorParent);
		addField(tst);

		Collection<RuntimeInfo> rs = RuntimeInfoManager.getDefault()
				.getRuntimes();
		String[][] runtimes = new String[rs.size()][2];
		Iterator<RuntimeInfo> it = rs.iterator();
		for (int i = 0; i < rs.size(); i++) {
			runtimes[i][0] = it.next().getVersion().asMajor().toString();
			runtimes[i][1] = runtimes[i][0];
		}
		addField(new ComboFieldEditor(
				ProjectPreferencesConstants.RUNTIME_VERSION,
				"Runtime version:", runtimes, fieldEditorParent));
	}

	@Override
	protected String getPageId() {
		return "org.erlide.core";
	}

	public void init(IWorkbench workbench) {
	}
}

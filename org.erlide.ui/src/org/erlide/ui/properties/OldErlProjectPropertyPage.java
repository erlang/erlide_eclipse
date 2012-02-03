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
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.erlide.backend.BackendCore;
import org.erlide.core.internal.model.root.ProjectPreferencesConstants;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlProject;
import org.erlide.jinterface.ErlLogger;

import com.bdaum.overlayPages.FieldEditorOverlayPage;

public class OldErlProjectPropertyPage extends FieldEditorOverlayPage {

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
        final IProject prj = (IProject) getElement().getAdapter(IProject.class);

        try {
            prj.getFolder(new Path(".settings")).refreshLocal(
                    IResource.DEPTH_ONE, null);
        } catch (final CoreException e) {
        }

        final Composite fieldEditorParent = getFieldEditorParent();
        final ProjectDirectoryFieldEditor out = new ProjectDirectoryFieldEditor(
                ProjectPreferencesConstants.OUTPUT_DIR, "Output directory:",
                fieldEditorParent, prj, false);
        addField(out);

        final ProjectPathEditor src = new ProjectPathEditor(
                ProjectPreferencesConstants.SOURCE_DIRS, "Source directories:",
                "Select directory:", fieldEditorParent, prj);
        addField(src);

        final ProjectPathEditor inc = new ProjectPathEditor(
                ProjectPreferencesConstants.INCLUDE_DIRS,
                "Include directories:", "Select directory:", fieldEditorParent,
                prj);
        addField(inc);

        // IPreferenceStore ps = getPreferenceStore();
        // OldErlangProjectProperties props = new
        // OldErlangProjectProperties(prj);
        // List<String> tstDirs = props.getTestDirs();
        // String tstStr = PreferencesUtils.packList(tstDirs);
        // ps.setValue(ProjectPreferencesConstants.TEST_DIRS, tstStr);
        //
        // ProjectPathEditor tst = new ProjectPathEditor(
        // ProjectPreferencesConstants.TEST_DIRS,
        // "Test source directories:", "Select directory:",
        // fieldEditorParent, prj);
        // tst.setEnabled(false, fieldEditorParent);
        // addField(tst);

        final String[][] runtimes = BackendCore.getRuntimeInfoManager()
                .getAllRuntimesVersions();
        addField(new ComboFieldEditor(
                ProjectPreferencesConstants.RUNTIME_VERSION,
                "Runtime version:", runtimes, fieldEditorParent));

        addField(new BooleanFieldEditor(
                ProjectPreferencesConstants.NUKE_OUTPUT_ON_CLEAN,
                "When cleaning, delete the whole output directories (is faster)",
                fieldEditorParent));
    }

    @Override
    protected String getPageId() {
        return "org.erlide.core";
    }

    @Override
    public void init(final IWorkbench workbench) {
    }

    @Override
    public boolean performOk() {
        final IProject project = (IProject) getElement().getAdapter(
                IProject.class);
        final IErlProject erlProject = ErlModelManager.getErlangModel()
                .getErlangProject(project);
        erlProject.clearCaches();
        return super.performOk();
    }
}

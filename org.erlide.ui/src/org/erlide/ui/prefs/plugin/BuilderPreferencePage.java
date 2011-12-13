/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs.plugin;

// import org.eclipse.jface.preference.PathEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * The system preferences
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class BuilderPreferencePage extends FieldEditorPreferencePage implements
        IWorkbenchPreferencePage {

    // private PathEditor pathA;

    // private PathEditor pathZ;

    // private BooleanFieldEditor longName;
    // private StringFieldEditor extra;

    /**
     * Initialize the system preferences
     * 
     */
    public BuilderPreferencePage() {
        super(FieldEditorPreferencePage.GRID);
    }

    /**
     * @see org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors()
     */
    @Override
    protected void createFieldEditors() {
        // pathZ = new PathEditor(PreferenceConstants.ERTS_PATH_Z,
        // ErlideBasicUIPlugin
        // .getResourceString("prefs.system.pathz"),
        // "Add a library directory to code:pathz()",
        // getFieldEditorParent());
        // addField(pathZ);

        // extra = new StringFieldEditor(PreferenceConstants.ERTS_EXTRA_ARGS,
        // ErlideBasicUIPlugin.getResourceString("prefs.system.extra"),
        // getFieldEditorParent());
        // addField(extra);

    }

    /**
     * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
    public void init(final IWorkbench workbench) {
    }

    @Override
    protected void performDefaults() {
        super.performDefaults();
    }

    @Override
    public boolean performOk() {
        return super.performOk();
    }

    @Override
    protected void initialize() {
        super.initialize();
        // ErtsPreferences prefs = ErlideBasicUIPlugin.getDefault()
        // .getPreferences();
        // home.setStringValue(prefs.getOtpHome());

        // pathA.setStringValue(prefs.getPathA());
        // pathZ.setStringValue(prefs.getPathZ());

        // longName.g(prefs.getErtsPrefs().getExtraErtsArgs());
        // setStringValue shouldn't be used
        // extra.setStringValue(prefs.getExtraErtsArgs());

    }

}

/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.properties;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PropertyPage;
import org.erlide.backend.BackendCore;
import org.erlide.core.services.search.ErlideDoc;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;
import org.erlide.ui.internal.ErlideUIPlugin;

/**
 * Property page used to set the project's edoc location
 */
public class EdocConfigurationPropertyPage extends PropertyPage implements
        IPreferenceChangeListener, IPropertyChangeListener {

    public static final String PROP_ID = "org.eclipse.jdt.ui.propertyPages.EdocConfigurationPropertyPage"; //$NON-NLS-1$

    // private boolean fIsValidElement;

    // private IPath fContainerPath;
    @SuppressWarnings("unused")
    private URL fInitialLocation;

    public EdocConfigurationPropertyPage() {
    }

    @Override
    protected IPreferenceStore doGetPreferenceStore() {
        final IPreferenceStore store = ErlideUIPlugin.getDefault()
                .getPreferenceStore();
        store.addPropertyChangeListener(this);
        return store;
    }

    /**
     * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public void createControl(final Composite parent) {
        super.createControl(parent);
        setDescription("Specify the location of the generated edoc (in HTML format).");
        PlatformUI
                .getWorkbench()
                .getHelpSystem()
                .setHelp(getControl(),
                        IErlangHelpContextIds.EDOC_CONFIGURATION_PROPERTY_PAGE);
    }

    /*
     * @see PreferencePage#createContents(Composite)
     */
    @Override
    protected Control createContents(final Composite parent) {
        fInitialLocation = null;
        final String s = ErlideDoc.getOtpDocLocation(BackendCore
                .getBackendManager().getIdeBackend());
        try {
            fInitialLocation = new URL("file", null, s);
        } catch (final MalformedURLException e) {
            ErlLogger.warn(e);
        }

        final Control control = new Composite(parent, SWT.NONE);
        // final Button button = new Button("test");
        // final Label label = new Label("Edoc location!!");

        Dialog.applyDialogFont(control);
        return control;
    }

    /*
     * @see PreferencePage#performDefaults()
     */
    @Override
    protected void performDefaults() {
        super.performDefaults();
    }

    /**
     * @see org.eclipse.jface.preference.IPreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        return true;
    }

    @Override
    public void preferenceChange(final PreferenceChangeEvent event) {
    }

    @Override
    public void propertyChange(final PropertyChangeEvent event) {
    }

}

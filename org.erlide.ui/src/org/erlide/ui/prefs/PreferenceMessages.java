/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.osgi.util.NLS;

public final class PreferenceMessages {
    private static final String BUNDLE_NAME = "org.erlide.ui.prefs.PreferenceMessages"; //$NON-NLS-1$

    public static String InstalledRuntimesBlock_name;
    public static String InstalledRuntimesBlock_location;
    public static String InstalledRuntimesBlock_version;
    public static String InstalledRuntimesBlock_add;
    public static String InstalledRuntimesBlock_edit;
    public static String InstalledRuntimesBlock_remove;
    public static String InstalledRuntimesBlock_search;
    public static String InstalledRuntimesBlock_add_title;
    public static String InstalledRuntimesBlock_edit_title;
    public static String InstalledRuntimesBlock_search_message;
    public static String InstalledRuntimesBlock_search_text;
    public static String InstalledRuntimesBlock_search_task;
    public static String InstalledRuntimesBlock_info_title;
    public static String InstalledRuntimesBlock_info_message;
    public static String InstalledRuntimesBlock_installedRuntimes;
    public static String RuntimesPreferencePage_title;
    public static String RuntimesPreferencePage_description;
    public static String RuntimesPreferencePage_pleaseSelectADefaultRuntime;
    public static String addRuntimeDialog_ertsName;
    public static String addRuntimeDialog_pickRuntimeRootDialog_message;
    public static String AddRuntimeDialog_add;
    public static String AddRuntimeDialog_remove;
    public static String ErlangPreferencePage_2;
    public static String ErlangPreferencePage_3;
    public static String ErlangPreferencePage_4;

    static {
        NLS.initializeMessages(BUNDLE_NAME, PreferenceMessages.class);
    }

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
            .getBundle(BUNDLE_NAME);

    private PreferenceMessages() {
    }

    public static String getString(final String key) {
        try {
            return RESOURCE_BUNDLE.getString(key);
        } catch (final MissingResourceException e) {
            return '!' + key + '!';
        }
    }

}

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

import org.eclipse.osgi.util.NLS;

public final class PreferencesMessages extends NLS {

    private static final String BUNDLE_NAME = "org.erlide.ui.prefs.plugin.PreferencesMessages"; //$NON-NLS-1$

    public static String ErlEditorPreferencePage_coloring_category_erlang;
    public static String ErlEditorPreferencePage_coloring_category_edoc;
    public static String ErlEditorColoringConfigurationBlock_link;
    public static String ErlEditorPreferencePage_coloring_element;
    public static String ErlEditorPreferencePage_enable;
    public static String ErlEditorPreferencePage_color;
    public static String ErlEditorPreferencePage_bold;
    public static String ErlEditorPreferencePage_italic;
    public static String ErlEditorPreferencePage_strikeout;
    public static String ErlEditorPreferencePage_underline;
    public static String ErlEditorPreferencePage_preview;
    public static String ErlEditorPreferencePage_folding_title;
    public static String FoldingConfigurationBlock_enable;
    public static String FoldingConfigurationBlock_combo_caption;
    public static String FoldingConfigurationBlock_info_no_preferences;
    public static String FoldingConfigurationBlock_error_not_exist;

    private PreferencesMessages() {
        // Do not instantiate
    }

    static {
        NLS.initializeMessages(BUNDLE_NAME, PreferencesMessages.class);
    }

}

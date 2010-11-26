/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs.plugin;

import org.eclipse.osgi.util.NLS;

/**
 * 
 * 
 * @author VlaDum
 */
public class ErlEditorMessages extends NLS {

    private static final String BUNDLE_NAME = "org.erlide.ui.prefs.plugin.ErlEditorMessages"; //$NON-NLS-1$

    public static String ErlEditorPreferencePage_description;
    public static String ErlEditorPreferencePage_lineNumberForegroundColor;
    public static String ErlEditorPreferencePage_currentLineHighlighColor;
    public static String ErlEditorPreferencePage_printMarginColor;
    public static String ErlEditorPreferencePage_selectionForegroundColor;
    public static String ErlEditorPreferencePage_selectionBackgroundColor;
    public static String ErlEditorPreferencePage_displayedTabWidth;
    public static String ErlEditorPreferencePage_printMarginColumn;
    public static String ErlEditorPreferencePage_showOverviewRuler;
    public static String ErlEditorPreferencePage_showLineNumbers;
    public static String ErlEditorPreferencePage_highlightCurrentLine;
    public static String ErlEditorPreferencePage_showPrintMargin;
    public static String ErlEditorPreferencePage_accessibility_disableCustomCarets;
    public static String ErlEditorPreferencePage_accessibility_wideCaret;
    public static String ErlEditorPreferencePage_appearanceOptions;
    public static String ErlEditorPreferencePage_color;
    public static String ErlEditorPreferencePage_systemDefault;
    public static String ErlEditorPreferencePage_empty_input;
    public static String ErlEditorPreferencePage_invalid_input;
    public static String ErlangOutlinePage_error_noelement;
    public static String Prefs_End_paren;
    public static String Prefs_Binary_begin;
    public static String Prefs_Paren;
    public static String Prefs_Before_binary_op;
    public static String Prefs_After_unary_op;
    public static String Prefs_Clause;
    public static String Prefs_Case;
    public static String Prefs_Try;
    public static String Prefs_Catch;
    public static String Prefs_Function_parameters;
    public static String Prefs_After_binary_op;
    public static String Prefs_Fun;
    public static String Prefs_Fun_body;
    public static String IndentationPrefs_36;
    public static String Prefs_Semicolon_nl;
    public static String Prefs_Dot_nl;
    public static String Prefs_Arrow_nl;
    public static String Prefs_Comma_nl;
    public static String Prefs_Before_arrow;
    public static String Prefs_After_arrow;
    public static String SmartTypingPrefs_EmbraceSelection;
    public static String SmartTypingPrefs_atoms;
    public static String SmartTypingPrefs_AutomaticallyClose;
    public static String SmartTypingPrefs_Braces;
    public static String SmartTypingPrefs_Brackets;
    public static String SmartTypingPrefs_Desc;
    public static String SmartTypingPrefs_Parens;
    public static String SmartTypingPrefs_Strings;
    public static String SmartTypingPrefs_WhenPasting;
    public static String SmartTypingPrefs_AdjustIndentation;
    public static String SmartTypingPrefs_autoNewLine;

    private ErlEditorMessages() {
    }

    static {
        NLS.initializeMessages(BUNDLE_NAME, ErlEditorMessages.class);
    }

}

package org.erlide.ui.templates;

import org.erlide.ui.internal.ErlideUIPlugin;

public class ErlTemplateCompletionPreferences {

    private static final boolean DEFAULT_INDENT_CODE = true;
    private static final String INDENT_CODE_PREFS_KEY = "templateIndentCode";

    private static boolean hasPrefs = false;
    private static boolean sIndentCode = DEFAULT_INDENT_CODE;

    public static boolean getIndentCode() {
        if (!hasPrefs) {
            getPrefs();
        }
        return sIndentCode;
    }

    public static void setIndentCode(final boolean selection) {
        sIndentCode = selection;
    }

    public static void getDefaults() {
        sIndentCode = DEFAULT_INDENT_CODE;
    }

    public static void putPrefs() {
        ErlideUIPlugin.getPrefsNode().putBoolean(INDENT_CODE_PREFS_KEY,
                sIndentCode);
    }

    public static void getPrefs() {
        sIndentCode = ErlideUIPlugin.getPrefsNode().getBoolean(
                INDENT_CODE_PREFS_KEY, DEFAULT_INDENT_CODE);
        hasPrefs = true;
    }

}

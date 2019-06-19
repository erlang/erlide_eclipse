package org.erlide.ui.templates;

import org.erlide.ui.internal.ErlideUIPlugin;

public class ErlTemplateCompletionPreferences {

    private static final boolean DEFAULT_INDENT_CODE = true;
    private static final String INDENT_CODE_PREFS_KEY = "templateIndentCode";

    private static boolean hasPrefs;
    private static boolean sIndentCode = ErlTemplateCompletionPreferences.DEFAULT_INDENT_CODE;

    public static boolean getIndentCode() {
        if (!ErlTemplateCompletionPreferences.hasPrefs) {
            ErlTemplateCompletionPreferences.getPrefs();
        }
        return ErlTemplateCompletionPreferences.sIndentCode;
    }

    public static void setIndentCode(final boolean selection) {
        ErlTemplateCompletionPreferences.sIndentCode = selection;
    }

    public static void getDefaults() {
        ErlTemplateCompletionPreferences.sIndentCode = ErlTemplateCompletionPreferences.DEFAULT_INDENT_CODE;
    }

    public static void putPrefs() {
        ErlideUIPlugin.getPrefsNode().putBoolean(
                ErlTemplateCompletionPreferences.INDENT_CODE_PREFS_KEY,
                ErlTemplateCompletionPreferences.sIndentCode);
    }

    public static void getPrefs() {
        ErlTemplateCompletionPreferences.sIndentCode = ErlideUIPlugin.getPrefsNode()
                .getBoolean(ErlTemplateCompletionPreferences.INDENT_CODE_PREFS_KEY,
                        ErlTemplateCompletionPreferences.DEFAULT_INDENT_CODE);
        ErlTemplateCompletionPreferences.hasPrefs = true;
    }

}

package org.erlide.ui.prefs;

import org.eclipse.osgi.util.NLS;

public final class RuntimePreferenceMessages extends NLS {
    private static final String BUNDLE_NAME = "org.erlide.ui.prefs.RuntimePreferenceMessages"; //$NON-NLS-1$

    public static String duplicate;
    public static String name;
    public static String location;
    public static String version;
    public static String add;
    public static String edit;
    public static String remove;
    public static String add_title;
    public static String edit_title;
    public static String info_title;
    public static String info_message;
    public static String installations;
    public static String Page_title;
    public static String Page_description;
    public static String Page_pleaseSelectADefaultRuntime;
    public static String addDialog_ertsName;
    public static String addDialog_add;
    public static String addDialog_remove;
    public static String addDialog_pickInstallationRoot;
    public static String RuntimePreferencePage_erlideLabel_text;

    static {
        NLS.initializeMessages(BUNDLE_NAME, RuntimePreferenceMessages.class);
    }

    private RuntimePreferenceMessages() {
    }
}

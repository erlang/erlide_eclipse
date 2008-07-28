package org.erlide.ui.prefs;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.osgi.util.NLS;

public class PreferenceMessages {
	private static final String BUNDLE_NAME = "org.erlide.ui.prefs.PreferenceMessages"; //$NON-NLS-1$

	public static String InstalledRuntimesBlock_0;
	public static String InstalledRuntimesBlock_1;
	public static String InstalledRuntimesBlock_2;
	public static String InstalledRuntimesBlock_3;
	public static String InstalledRuntimesBlock_4;
	public static String InstalledRuntimesBlock_5;
	public static String InstalledRuntimesBlock_6;
	public static String InstalledRuntimesBlock_7;
	public static String InstalledRuntimesBlock_8;
	public static String InstalledRuntimesBlock_9;
	public static String InstalledRuntimesBlock_10;
	public static String InstalledRuntimesBlock_11;
	public static String InstalledRuntimesBlock_12;
	public static String InstalledRuntimesBlock_13;
	public static String InstalledRuntimesBlock_15;
	public static String RuntimesPreferencePage_1;
	public static String RuntimesPreferencePage_2;
	public static String RuntimesPreferencePage_13;
	public static String addRuntimeDialog_ertsName;
	public static String addRuntimeDialog_pickRuntimeRootDialog_message;
	public static String AddRuntimeDialog_3;
	public static String AddRuntimeDialog_5;

	static {
		NLS.initializeMessages(BUNDLE_NAME, PreferenceMessages.class);
	}

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
			.getBundle(BUNDLE_NAME);

	private PreferenceMessages() {
	}

	public static String getString(String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		} catch (MissingResourceException e) {
			return '!' + key + '!';
		}
	}

}

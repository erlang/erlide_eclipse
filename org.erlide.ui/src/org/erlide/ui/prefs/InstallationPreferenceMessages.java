package org.erlide.ui.prefs;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.osgi.util.NLS;

public class InstallationPreferenceMessages {
	private static final String BUNDLE_NAME = "org.erlide.ui.prefs.InstallationPreferenceMessages"; //$NON-NLS-1$

	public static String name;
	public static String location;
	public static String version;
	public static String add;
	public static String edit;
	public static String remove;
	public static String search;
	public static String add_title;
	public static String edit_title;
	public static String search_message;
	public static String search_text;
	public static String search_task;
	public static String info_title;
	public static String info_message;
	public static String installations;
	public static String Page_title;
	public static String Page_description;
	public static String Page_pleaseSelectADefaultInstallation;
	public static String addDialog_ertsName;
	public static String addDialog_pickInstallationRootDialog;
	public static String addDialog_add;
	public static String addDialog_remove;

	static {
		NLS.initializeMessages(BUNDLE_NAME,
				InstallationPreferenceMessages.class);
	}

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
			.getBundle(BUNDLE_NAME);

	private InstallationPreferenceMessages() {
	}

	public static String getString(String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		} catch (MissingResourceException e) {
			return '!' + key + '!';
		}
	}

}

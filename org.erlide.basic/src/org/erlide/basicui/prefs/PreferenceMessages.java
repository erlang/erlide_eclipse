/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.basicui.prefs;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.osgi.util.NLS;

public class PreferenceMessages {
	private static final String BUNDLE_NAME = "org.erlide.basicui.prefs.PreferenceMessages"; //$NON-NLS-1$

	public static String InstalledERTSsBlock_0;
	public static String InstalledERTSsBlock_1;
	public static String InstalledERTSsBlock_2;
	public static String InstalledERTSsBlock_3;
	public static String InstalledERTSsBlock_4;
	public static String InstalledERTSsBlock_5;
	public static String InstalledERTSsBlock_6;
	public static String InstalledERTSsBlock_7;
	public static String InstalledERTSsBlock_8;
	public static String InstalledERTSsBlock_9;
	public static String InstalledERTSsBlock_10;
	public static String InstalledERTSsBlock_11;
	public static String InstalledERTSsBlock_12;
	public static String InstalledERTSsBlock_13;
	public static String InstalledERTSsBlock_15;
	public static String ERTSsPreferencePage_1;
	public static String ERTSsPreferencePage_2;
	public static String ERTSsPreferencePage_13;
	public static String addVMDialog_ertsName;
	public static String addVMDialog_pickERTSRootDialog_message;
	public static String AddVMDialog_3;
	public static String AddVMDialog_5;

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

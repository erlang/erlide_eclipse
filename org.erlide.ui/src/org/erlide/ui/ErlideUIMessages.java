/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.osgi.util.NLS;

public class ErlideUIMessages {

	private static final String BUNDLE_NAME = "org.erlide.ui.ErlideUIMessages";//$NON-NLS-1$

	private static final ResourceBundle resourceBundle = ResourceBundle
			.getBundle(BUNDLE_NAME);

	public static int ExceptionHandler_seeErrorLogMessage;

	static {
		// load message values from bundle file
		NLS.initializeMessages(BUNDLE_NAME, ErlideUIMessages.class);
	}

	public static String getString(String key) {
		try {
			return resourceBundle.getString(key);
		} catch (final MissingResourceException e) {
			return '!' + key + '!';
		}
	}

	public static String getFormattedString(String key, String arg) {
		return getFormattedString(key, new String[] { arg });
	}

	public static String getFormattedString(String key, String[] args) {
		return MessageFormat.format(getString(key), (Object[]) args);
	}

	public static ResourceBundle getResourceBundle() {
		return resourceBundle;
	}
}

/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.ui.prefs;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.erlide.ui.ErlideUIPlugin;

/**
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class PreferencesInitializer extends AbstractPreferenceInitializer {

	/**
	 * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
	 */
	@Override
	public void initializeDefaultPreferences() {
		final IPreferenceStore store = ErlideUIPlugin.getDefault()
				.getPreferenceStore();

		PreferenceConstants.initializeDefaultValues(store);

		PreferenceConverter.setDefault(store, PreferenceConstants.ATTRIBUTE,
				PreferenceConstants.DEFAULT_ATTRIBUTE_COLOR);
		PreferenceConverter.setDefault(store, PreferenceConstants.COMMENT,
				PreferenceConstants.DEFAULT_COMMENT_COLOR);
		PreferenceConverter.setDefault(store, PreferenceConstants.DEFAULT,
				PreferenceConstants.DEFAULT_DEFAULT_COLOR);
		PreferenceConverter.setDefault(store, PreferenceConstants.KEYWORD,
				PreferenceConstants.DEFAULT_KEYWORD_COLOR);
		PreferenceConverter.setDefault(store, PreferenceConstants.STRING,
				PreferenceConstants.DEFAULT_STRING_COLOR);
		PreferenceConverter.setDefault(store, PreferenceConstants.VARIABLE,
				PreferenceConstants.DEFAULT_VARIABLE_COLOR);
		PreferenceConverter.setDefault(store, PreferenceConstants.ARROW,
				PreferenceConstants.DEFAULT_ARROW_COLOR);
		PreferenceConverter.setDefault(store, PreferenceConstants.BIF,
				PreferenceConstants.DEFAULT_BIF_COLOR);
		PreferenceConverter.setDefault(store, PreferenceConstants.GUARD,
				PreferenceConstants.DEFAULT_GUARD_COLOR);
		PreferenceConverter.setDefault(store, PreferenceConstants.MACRO,
				PreferenceConstants.DEFAULT_MACRO_COLOR);
		PreferenceConverter.setDefault(store, PreferenceConstants.RECORD,
				PreferenceConstants.DEFAULT_RECORD_COLOR);
		PreferenceConverter.setDefault(store, PreferenceConstants.CHAR,
				PreferenceConstants.DEFAULT_CHAR_COLOR);
		PreferenceConverter.setDefault(store, PreferenceConstants.ATOM,
				PreferenceConstants.DEFAULT_ATOM_COLOR);
		PreferenceConverter.setDefault(store, PreferenceConstants.INTEGER,
				PreferenceConstants.DEFAULT_INTEGER_COLOR);
		PreferenceConverter.setDefault(store, PreferenceConstants.FLOAT,
				PreferenceConstants.DEFAULT_FLOAT_COLOR);

		store.setDefault(PreferenceConstants.PRINT_MARGIN,
				PreferenceConstants.DEFAULT_PRINT_MARGIN);

	}

}

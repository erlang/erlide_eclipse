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

import static org.erlide.ui.prefs.PreferenceConstants.ARROW;
import static org.erlide.ui.prefs.PreferenceConstants.ATOM;
import static org.erlide.ui.prefs.PreferenceConstants.ATTRIBUTE;
import static org.erlide.ui.prefs.PreferenceConstants.BIF;
import static org.erlide.ui.prefs.PreferenceConstants.CHAR;
import static org.erlide.ui.prefs.PreferenceConstants.COMMENT;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_ARROW_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_ATOM_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_ATTRIBUTE_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_BIF_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_CHAR_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_COMMENT_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_DEFAULT_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_FLOAT_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_GUARD_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_INTEGER_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_KEYWORD_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_MACRO_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_PRINT_MARGIN;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_RECORD_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_STRING_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.DEFAULT_VARIABLE_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.EDITOR_FOLDING_CLAUSES;
import static org.erlide.ui.prefs.PreferenceConstants.EDITOR_FOLDING_COMMENTS;
import static org.erlide.ui.prefs.PreferenceConstants.EDITOR_FOLDING_EDOC;
import static org.erlide.ui.prefs.PreferenceConstants.EDITOR_FOLDING_ENABLED;
import static org.erlide.ui.prefs.PreferenceConstants.EDITOR_FOLDING_HEADERS;
import static org.erlide.ui.prefs.PreferenceConstants.EDITOR_FOLDING_PROVIDER;
import static org.erlide.ui.prefs.PreferenceConstants.EDITOR_MATCHING_BRACKETS;
import static org.erlide.ui.prefs.PreferenceConstants.EDITOR_MATCHING_BRACKETS_COLOR;
import static org.erlide.ui.prefs.PreferenceConstants.FLOAT;
import static org.erlide.ui.prefs.PreferenceConstants.GUARD;
import static org.erlide.ui.prefs.PreferenceConstants.INTEGER;
import static org.erlide.ui.prefs.PreferenceConstants.KEYWORD;
import static org.erlide.ui.prefs.PreferenceConstants.MACRO;
import static org.erlide.ui.prefs.PreferenceConstants.PRINT_MARGIN;
import static org.erlide.ui.prefs.PreferenceConstants.RECORD;
import static org.erlide.ui.prefs.PreferenceConstants.STRING;
import static org.erlide.ui.prefs.PreferenceConstants.VARIABLE;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.graphics.RGB;
import org.erlide.ui.ErlideUIPlugin;

/**
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class PreferencesInitializer extends AbstractPreferenceInitializer {

	@Override
	public void initializeDefaultPreferences() {
		final IPreferenceStore store = ErlideUIPlugin.getDefault()
				.getPreferenceStore();

		PreferenceConverter.setDefault(store, ATTRIBUTE,
				DEFAULT_ATTRIBUTE_COLOR);
		PreferenceConverter.setDefault(store, COMMENT, DEFAULT_COMMENT_COLOR);
		PreferenceConverter.setDefault(store, DEFAULT, DEFAULT_DEFAULT_COLOR);
		PreferenceConverter.setDefault(store, KEYWORD, DEFAULT_KEYWORD_COLOR);
		PreferenceConverter.setDefault(store, STRING, DEFAULT_STRING_COLOR);
		PreferenceConverter.setDefault(store, VARIABLE, DEFAULT_VARIABLE_COLOR);
		PreferenceConverter.setDefault(store, ARROW, DEFAULT_ARROW_COLOR);
		PreferenceConverter.setDefault(store, BIF, DEFAULT_BIF_COLOR);
		PreferenceConverter.setDefault(store, GUARD, DEFAULT_GUARD_COLOR);
		PreferenceConverter.setDefault(store, MACRO, DEFAULT_MACRO_COLOR);
		PreferenceConverter.setDefault(store, RECORD, DEFAULT_RECORD_COLOR);
		PreferenceConverter.setDefault(store, CHAR, DEFAULT_CHAR_COLOR);
		PreferenceConverter.setDefault(store, ATOM, DEFAULT_ATOM_COLOR);
		PreferenceConverter.setDefault(store, INTEGER, DEFAULT_INTEGER_COLOR);
		PreferenceConverter.setDefault(store, FLOAT, DEFAULT_FLOAT_COLOR);

		store.setDefault(PRINT_MARGIN, DEFAULT_PRINT_MARGIN);

		store.setDefault(EDITOR_MATCHING_BRACKETS, true);
		PreferenceConverter.setDefault(store, EDITOR_MATCHING_BRACKETS_COLOR,
				new RGB(190, 140, 190));

		// folding
		store.setDefault(EDITOR_FOLDING_ENABLED, true);
		store.setDefault(EDITOR_FOLDING_PROVIDER,
				"org.erlide.ui.editors.defaultFoldingProvider");
		store.setDefault(EDITOR_FOLDING_EDOC, false);
		store.setDefault(EDITOR_FOLDING_HEADERS, true);
		store.setDefault(EDITOR_FOLDING_COMMENTS, false);
		store.setDefault(EDITOR_FOLDING_CLAUSES, false);
		store.setDefault(EDITOR_FOLDING_HEADERS, true);
	}

}

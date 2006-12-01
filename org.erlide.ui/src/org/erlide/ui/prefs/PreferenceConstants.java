/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.graphics.RGB;

/**
 * Constants used in project and plugin preferences
 * 
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com]
 */
public class PreferenceConstants {

	public static final String EDITOR_PREFIX = "__erl_editor_";

	// Editor colors
	public static final String DEFAULT = EDITOR_PREFIX + "default";

	public static final String KEYWORD = EDITOR_PREFIX + "keyword";

	public static final String ATOM = EDITOR_PREFIX + "atom";

	public static final String RECORD = EDITOR_PREFIX + "record";

	public static final String MACRO = EDITOR_PREFIX + "macro";

	public static final String BIF = EDITOR_PREFIX + "bif";

	public static final String GUARD = EDITOR_PREFIX + "guard";

	public static final String ARROW = EDITOR_PREFIX + "arrow";

	public static final String CHAR = EDITOR_PREFIX + "char";

	public static final String VARIABLE = EDITOR_PREFIX + "variable";

	public static final String COMMENT = EDITOR_PREFIX + "comment";

	public static final String ATTRIBUTE = EDITOR_PREFIX + "attribute";

	public static final String STRING = EDITOR_PREFIX + "string";

	public static final String INTEGER = EDITOR_PREFIX + "integer";

	public static final String FLOAT = EDITOR_PREFIX + "float";

	// The default editor colors

	public static final RGB DEFAULT_COMMENT_COLOR = new RGB(0, 115, 0);

	public static final RGB DEFAULT_ATTRIBUTE_COLOR = new RGB(0, 0, 255);

	public static final RGB DEFAULT_STRING_COLOR = new RGB(158, 28, 160);

	public static final RGB DEFAULT_DEFAULT_COLOR = new RGB(50, 50, 90);

	public static final RGB DEFAULT_KEYWORD_COLOR = new RGB(128, 0, 255);

	public static final RGB DEFAULT_VARIABLE_COLOR = new RGB(150, 100, 0);

	public static final RGB DEFAULT_FUNCTION_COLOR = new RGB(0, 150, 120);

	public static final RGB DEFAULT_GUARD_COLOR = new RGB(50, 80, 150);

	public static final RGB DEFAULT_MACRO_COLOR = new RGB(50, 150, 50);

	public static final RGB DEFAULT_RECORD_COLOR = new RGB(90, 150, 0);

	public static final RGB DEFAULT_BIF_COLOR = new RGB(0, 80, 150);

	public static final RGB DEFAULT_CHAR_COLOR = new RGB(200, 0, 250);

	public static final RGB DEFAULT_ATOM_COLOR = new RGB(0, 0, 0);

	public static final RGB DEFAULT_ARROW_COLOR = new RGB(0, 50, 230);

	public static final RGB DEFAULT_INTEGER_COLOR = new RGB(90, 90, 180);

	public static final RGB DEFAULT_FLOAT_COLOR = new RGB(90, 90, 220);

	/*
	 * PROJECT PREFERENCES
	 */

	public static final int DEFAULT_PRINT_MARGIN = 80;

	public static final String PRINT_MARGIN = "__erl_print_margin";

	public static final String EDITOR_TEXT_FONT = "__erl_text_font";

	public static final String EDITOR_COMMENT_COLOR = "";

	public static final String EDITOR_ATTRIBUTE_COLOR = "";

	public static final String EDITOR_STRING_COLOR = "";

	public static final String EDITOR_DEFAULT_COLOR = "";

	public static final String EDITOR_KEYWORD_COLOR = "";

	public static final String EDITOR_VARIABLE_COLOR = "";

	public static final String EDITOR_FUNCTION_COLOR = "";

	public static final String EDITOR_GUARD_COLOR = "";

	public static final String EDITOR_MACRO_COLOR = "";

	public static final String EDITOR_RECORD_COLOR = "";

	public static final String EDITOR_BIF_COLOR = "";

	public static final String EDITOR_CHAR_COLOR = "";

	public static final String EDITOR_ATOM_COLOR = "";

	public static final String EDITOR_ARROW_COLOR = "";

	public static final String EDITOR_INTEGER_COLOR = "";

	public static final String EDITOR_FLOAT_COLOR = "";

	/**
	 * Preference key suffix for bold text style preference keys.
	 * 
	 * @since 2.1
	 */
	public static final String EDITOR_BOLD_SUFFIX = "_bold"; //$NON-NLS-1$

	/**
	 * Preference key suffix for italic text style preference keys.
	 * 
	 * @since 3.0
	 */
	public static final String EDITOR_ITALIC_SUFFIX = "_italic"; //$NON-NLS-1$

	/**
	 * Preference key suffix for strikethrough text style preference keys.
	 * 
	 * @since 3.1
	 */
	public static final String EDITOR_STRIKETHROUGH_SUFFIX = "_strikethrough"; //$NON-NLS-1$

	/**
	 * Preference key suffix for underline text style preference keys.
	 * 
	 * @since 3.1
	 */
	public static final String EDITOR_UNDERLINE_SUFFIX = "_underline"; //$NON-NLS-1$

	/**
	 * A named preference that controls whether bracket matching highlighting is
	 * turned on or off.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 */
	public static final String EDITOR_MATCHING_BRACKETS = "matchingBrackets"; //$NON-NLS-1$

	// JC: Folding stuff (ripped from JDT)
	/**
	 * A named preference that controls whether folding is enabled in the
	 * editor.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 */
	public static final String EDITOR_FOLDING_ENABLED = "editor_folding_enabled"; //$NON-NLS-1$

	/**
	 * A named preference that stores the configured folding provider.
	 * <p>
	 * Value is of type <code>String</code>.
	 * </p>
	 */
	public static final String EDITOR_FOLDING_PROVIDER = "editor_folding_provider"; //$NON-NLS-1$

	/**
	 * A named preference that stores the value for edoc folding for the default
	 * folding provider.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 */
	public static final String EDITOR_FOLDING_EDOC = "editor_folding_default_edoc"; //$NON-NLS-1$

	/**
	 * A named preference that stores the value for function clauses folding for
	 * the default folding provider.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 */
	public static final String EDITOR_FOLDING_CLAUSES = "editor_folding_default_clauses"; //$NON-NLS-1$

	/**
	 * A named preference that stores the value for comment folding for the
	 * default folding provider.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 */
	public static final String EDITOR_FOLDING_COMMENTS = "editor_folding_default_comments"; //$NON-NLS-1$

	/**
	 * A named preference that stores the value for header comment folding for
	 * the default folding provider.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 */
	public static final String EDITOR_FOLDING_HEADERS = "editor_folding_default_headers"; //$NON-NLS-1$

	/**
	 * A named preference that holds the color used to highlight matching
	 * brackets.
	 * <p>
	 * Value is of type <code>String</code>. A RGB color value encoded as a
	 * string using class <code>PreferenceConverter</code>
	 * </p>
	 * 
	 * @see org.eclipse.jface.resource.StringConverter
	 * @see org.eclipse.jface.preference.PreferenceConverter
	 */
	public static final String EDITOR_MATCHING_BRACKETS_COLOR = "matchingBracketsColor"; //$NON-NLS-1$

	public static final String EDITOR_SHOW_SEGMENTS = "showSegments"; //$NON-NLS-1$

	public static void initializeDefaultValues(IPreferenceStore store) {
		store.setDefault(EDITOR_MATCHING_BRACKETS, true);
		PreferenceConverter.setDefault(store, EDITOR_MATCHING_BRACKETS_COLOR,
				new RGB(190, 140, 190));

		// folding
		store.setDefault(PreferenceConstants.EDITOR_FOLDING_ENABLED, true);
		store.setDefault(PreferenceConstants.EDITOR_FOLDING_PROVIDER,
				"org.erlide.ui.editors.defaultFoldingProvider"); //$NON-NLS-1$
		store.setDefault(PreferenceConstants.EDITOR_FOLDING_EDOC, false);
		store.setDefault(PreferenceConstants.EDITOR_FOLDING_HEADERS, true);
		store.setDefault(PreferenceConstants.EDITOR_FOLDING_COMMENTS, false);
		store.setDefault(PreferenceConstants.EDITOR_FOLDING_CLAUSES, false);
		store.setDefault(PreferenceConstants.EDITOR_FOLDING_HEADERS, true);
	}
}

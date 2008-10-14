/**
 * 
 */
package org.erlide.ui.prefs;

public enum Highlight {
	DEFAULT, KEYWORD, ATOM, RECORD, MACRO, BIF, GUARD, ARROW, CHAR, VARIABLE, COMMENT, ATTRIBUTE, STRING, INTEGER, FLOAT;
	private final String key;

	private Highlight() {
		key = PreferenceConstants.EDITOR_PREFIX + toString().toLowerCase();
	}

	public String getKey() {
		return key;
	}
}
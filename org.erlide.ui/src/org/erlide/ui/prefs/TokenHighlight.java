/**
 *
 */
package org.erlide.ui.prefs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.erlide.ui.prefs.PreferenceConstants.Color;

public enum TokenHighlight {
	DEFAULT(Color.BLACK.getColor(), SWT.NORMAL),

	KEYWORD(Color.BLACK.getColor(), SWT.BOLD),

	ATOM(Color.BLACK.getColor(), SWT.NORMAL),

	MACRO(new RGB(50, 150, 50), SWT.NORMAL),

	ARROW(new RGB(0, 50, 230), SWT.NORMAL),

	CHAR(new RGB(200, 0, 250), SWT.NORMAL),

	VARIABLE(new RGB(150, 100, 0), SWT.NORMAL),

	COMMENT(Color.DARKGREEN.getColor(), SWT.NORMAL),

	STRING(Color.DARKORCHID.getColor(), SWT.NORMAL),

	INTEGER(new RGB(90, 90, 180), SWT.NORMAL),

	FLOAT(Color.NAVY.getColor(), SWT.NORMAL);

	private final HighlightStyle defaultData;

	private TokenHighlight(RGB color, int style) {
		defaultData = new HighlightStyle(color, style);
	}

	public HighlightStyle getDefaultData() {
		return defaultData;
	}

	public String getName() {
		return toString().toLowerCase();
	}
}
package org.erlide.ui.prefs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.erlide.ui.prefs.PreferenceConstants.Color;

public enum TokenHighlight {
    //@formatter:off
    DEFAULT(Color.BLACK.getColor(), SWT.NORMAL),
    KEYWORD(new RGB(0xA0, 0x20, 0xF0), SWT.BOLD),
    ATOM(Color.BLACK.getColor(), SWT.NORMAL),
    MACRO(new RGB(0x5f, 0x9e, 0xA0), SWT.NORMAL),
    ARROW(new RGB(0x00, 0x00, 0xff), SWT.NORMAL),
    CHAR(new RGB(0xBc, 0x8f, 0x8F), SWT.NORMAL),
    VARIABLE(new RGB(0xB8, 0x86, 0x0B), SWT.NORMAL),
    INTEGER(new RGB(90, 90, 180), SWT.NORMAL),
    FLOAT(Color.NAVY.getColor(), SWT.NORMAL),
    COMMENT(new RGB(0xB2, 0x22, 0x22), SWT.NORMAL),
    EDOC_TAG(new RGB(0x82, 0x22, 0x22), SWT.BOLD, "EDoc tag (in comments)"),
    HTML_TAG(new RGB(0xB3, 0x6A, 0x6A), SWT.NORMAL, "HTML tag (in comments)"),
    STRING(new RGB(0xBC, 0x8f, 0x8F), SWT.NORMAL),
    ESCAPE_TAG(new RGB(0xBC, 0x8f, 0x8F), SWT.BOLD, "Escaped chars (in strings)"),
    TILDE_TAG(new RGB(0xBC, 0x8f, 0x8F), SWT.BOLD, "Format specifiers (in strings)");
    //@formatter:on

    private final HighlightStyle defaultData;
    private String displayName;

    private TokenHighlight(final RGB color, final int style) {
        this(color, style, null);
    }

    private TokenHighlight(final RGB color, final int style,
            final String displayName) {
        defaultData = new HighlightStyle(color, style);
        this.displayName = displayName;
    }

    public HighlightStyle getDefaultData() {
        return defaultData;
    }

    public String getName() {
        if (displayName == null) {
            return toString().toLowerCase().replaceAll("_", " ");
        }
        return displayName;
    }
}

package org.erlide.ui.prefs;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.graphics.RGB;
import org.erlide.ui.prefs.plugin.ColoringPreferencePage;

public enum TokenHighlight {
    // TODO do we keep the defaults? The css theme is always used.
    //@formatter:off
    DEFAULT("0072bc", 0), // Function
    KEYWORD("ff8d43", 1), // Keyword
    ATOM("4a4543", 0), // Real Foreground
    MACRO("bb7334", 0), // Class/Operator ?
    ARROW("0072bc", 1), // Function
    CHAR("579b57", 0),  // Foreground
    VARIABLE("db2d20", 0), // Variable
    INTEGER("11a0e4", 0), // Number
    FLOAT("11a0e4", 0), // Number

    COMMENT("9fa357", 0), // Comment
    EDOC_TAG("9fa357", 1, "EDoc tag (in comments)"), // Comment
    HTML_TAG("9fa357", 2, "HTML tag (in comments)"), // Comment

    STRING("11a0e4", 0), // String
    ESCAPE_TAG("11a0e4", 1, "Escaped chars (in strings)"), // String
    TILDE_TAG("11a0e4", 1, "Format specifiers (in strings)"); // String
    //@formatter:on

    private final RGB defaultColor;
    int defaultStyle;
    private final String displayName;

    TokenHighlight(final RGB defaultColor, final int defaultStyle) {
        this(defaultColor, defaultStyle, null);
    }

    TokenHighlight(final RGB defaultColor, final int defaultStyle,
            final String displayName) {
        this.defaultColor = defaultColor;
        this.defaultStyle = defaultStyle;
        this.displayName = displayName;
    }

    TokenHighlight(final String defaultColor, final int defaultStyle) {
        this(defaultColor, defaultStyle, null);
    }

    TokenHighlight(final String defaultColor, final int defaultStyle,
            final String displayName) {
        this.defaultColor = getRgbFromCss(defaultColor);
        this.defaultStyle = defaultStyle;
        this.displayName = displayName;
    }

    public static RGB getRgbFromCss(final String color) {
        int i;
        int s;
        int f;
        switch (color.length()) {
        case 3:
            i = 0;
            s = 1;
            f = 17;
            break;
        case 4:
            i = 1;
            s = 1;
            f = 17;
            break;
        case 6:
            i = 0;
            s = 2;
            f = 1;
            break;
        case 7:
            i = 1;
            s = 2;
            f = 1;
            break;
        default:
            throw new IllegalArgumentException(
                    "Unrecognizable CSS color string: '" + color + "'");
        }
        final Integer r = Integer.valueOf(color.substring(i, i + s), 16) * f;
        final Integer g = Integer.valueOf(color.substring(i + s, i + 2 * s), 16) * f;
        final Integer b = Integer.valueOf(color.substring(i + 2 * s, i + 3 * s), 16) * f;
        return new RGB(r, g, b);
    }

    public String getName() {
        return toString().toLowerCase();
    }

    public String getDisplayName() {
        if (displayName == null) {
            return getName().replaceAll("_", " ");
        }
        return displayName;
    }

    public String getStylesKey() {
        return ColoringPreferencePage.COLORS_QUALIFIER + getName() + "_"
                + ColoringPreferencePage.STYLE_KEY;
    }

    public String getColorKey() {
        return ColoringPreferencePage.COLORS_QUALIFIER + getName() + "_"
                + ColoringPreferencePage.COLOR_KEY;
    }

    public HighlightStyle getStyle(final IPreferenceStore store) {
        final String colorString = store.getString(getColorKey());
        RGB color;
        int styles = store.getInt(getStylesKey());
        try {
            color = getRgbFromCss(colorString);
        } catch (final Exception e) {
            try {
                final String[] rgb = colorString.split(",");
                color = new RGB(Integer.valueOf(rgb[0]), Integer.valueOf(rgb[1]),
                        Integer.valueOf(rgb[2]));
            } catch (final Exception e1) {
                color = defaultColor;
                styles = defaultStyle;
            }
        }
        return new HighlightStyle(color, styles);
    }

    public static boolean isColorKey(final String key) {
        if (!key.startsWith(ColoringPreferencePage.COLORS_QUALIFIER)) {
            return false;
        }
        return key.endsWith(ColoringPreferencePage.COLOR_KEY);
    }

    public static boolean isStylesKey(final String key) {
        if (!key.startsWith(ColoringPreferencePage.COLORS_QUALIFIER)) {
            return false;
        }
        return key.endsWith(ColoringPreferencePage.STYLE_KEY);
    }

    public static String getKeyName(final String key) {
        if (!key.startsWith(ColoringPreferencePage.COLORS_QUALIFIER)) {
            return null;
        }
        if (key.endsWith(ColoringPreferencePage.COLOR_KEY)) {
            return key.substring(ColoringPreferencePage.COLORS_QUALIFIER.length(),
                    key.length() - ColoringPreferencePage.COLOR_KEY.length() - 1);
        }
        return key.substring(ColoringPreferencePage.COLORS_QUALIFIER.length(),
                key.length() - ColoringPreferencePage.STYLE_KEY.length() - 1);
    }

}

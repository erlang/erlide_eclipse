package org.erlide.ui.prefs;

import java.io.IOException;
import java.io.InputStream;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.graphics.RGB;
import org.erlide.ui.prefs.plugin.ColoringPreferencePage;
import org.osgi.service.prefs.Preferences;

public enum TokenHighlight {
    //@formatter:off
    DEFAULT("4271ae", 0),
    KEYWORD("8959a8", 1),
    ATOM(new RGB(77,77,76), 0),
    MACRO(new RGB(234,183,0), 0),
    ARROW(new RGB(66,113,174), 1),
    CHAR(new RGB(113,140,0), 0),
    VARIABLE(new RGB(200,40,41), 0),
    INTEGER(new RGB( 245,135,31), 0),
    FLOAT(new RGB( 245,135,31), 0),

    COMMENT(new RGB(142,144,140), 0),
    EDOC_TAG(new RGB(142,144,140), 1, "EDoc tag (in comments)"),
    HTML_TAG(new RGB(142,144,140), 2, "HTML tag (in comments)"),

    STRING(new RGB(113,140,0), 0),
    ESCAPE_TAG(new RGB(113,140,0), 1, "Escaped chars (in strings)"),
    TILDE_TAG(new RGB(113,140,0), 1, "Format specifiers (in strings)");
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

    public static String getValueFromCss(final String css, final String key,
            final String kind) {
        final Pattern pattern = Pattern.compile(
                ".*'editor_colors_" + key + "_" + kind + " = ([^']+)'.*", Pattern.DOTALL);
        System.out.println(pattern);
        System.out.println(css);
        final Matcher m = pattern.matcher(css);
        if (m.matches()) {
            return m.group(1);
        }
        return null;
    }

    public static String getCssTheme() {
        // IWorkbench workbench = PlatformUI.getWorkbench();
        // ITheme theme = workbench.getThemeManager().getCurrentTheme();
        // System.out.println(theme.getColorRegistry().getKeySet());
        try (InputStream is = TokenHighlight.class
                .getResourceAsStream("/css/light_highlighting.css");
                Scanner s = new Scanner(is)) {
            s.useDelimiter("\\A");
            return s.hasNext() ? s.next() : "";
        } catch (final IOException e) {
            e.printStackTrace();
            return "";
        }
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
        final IEclipsePreferences node = InstanceScope.INSTANCE
                .getNode(ColoringPreferencePage.OLD_COLORS_QUALIFIER + "/" + getName());
        if (node != null) {
            final String colorString = node.get(ColoringPreferencePage.COLOR_KEY, null);
            if (colorString != null) {
                store.setValue(getColorKey(), colorString);
            }
            final int styles = node.getInt(ColoringPreferencePage.STYLE_KEY, -1);
            if (styles != -1) {
                store.setValue(getStylesKey(), styles);
            }
            try {
                final Preferences parent = node.parent();
                node.removeNode();
                parent.sync();
            } catch (final Exception e) {
                // ignore
            }
        }

        final String colorString = store.getString(getColorKey());
        RGB color;
        int styles;
        try {
            color = StringConverter.asRGB(colorString);
            styles = store.getInt(getStylesKey());
        } catch (final Exception e) {
            color = defaultColor;
            styles = defaultStyle;
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

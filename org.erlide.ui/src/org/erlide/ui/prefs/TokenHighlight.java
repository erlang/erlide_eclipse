package org.erlide.ui.prefs;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.erlide.ui.prefs.PreferenceConstants.Color;
import org.erlide.ui.prefs.plugin.ColoringPreferencePage;
import org.osgi.service.prefs.Preferences;

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

    private final HighlightStyle defaultStyle;
    private final String displayName;

    private TokenHighlight(final RGB color, final int styles) {
        this(color, styles, null);
    }

    private TokenHighlight(final RGB color, final int styles, final String displayName) {
        defaultStyle = new HighlightStyle(color, styles);
        this.displayName = displayName;
    }

    public HighlightStyle getDefaultStyle() {
        return defaultStyle;
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
        final RGB color = StringConverter.asRGB(colorString);
        final int styles = store.getInt(getStylesKey());
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

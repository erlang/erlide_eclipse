package org.erlide.ui.prefs;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.graphics.RGB;
import org.erlide.ui.prefs.plugin.ColoringPreferencePage;
import org.osgi.service.prefs.Preferences;

public enum TokenHighlight {
    //@formatter:off
    DEFAULT,
    KEYWORD,
    ATOM,
    MACRO,
    ARROW,
    CHAR,
    VARIABLE,
    INTEGER,
    FLOAT,

    COMMENT,
    EDOC_TAG("EDoc tag (in comments)"),
    HTML_TAG("HTML tag (in comments)"),

    STRING,
    ESCAPE_TAG("Escaped chars (in strings)"),
    TILDE_TAG("Format specifiers (in strings)");
    //@formatter:on

    private final String displayName;

    private TokenHighlight() {
        this(null);
    }

    private TokenHighlight(final String displayName) {
        this.displayName = displayName;
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

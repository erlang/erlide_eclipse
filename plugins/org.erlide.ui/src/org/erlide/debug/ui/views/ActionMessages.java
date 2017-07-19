package org.erlide.debug.ui.views;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

public final class ActionMessages {
    private static final String BUNDLE_NAME = "org.erlide.debug.ui.views.actionmessages"; //$NON-NLS-1$

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
            .getBundle(BUNDLE_NAME);

    private ActionMessages() {
    }

    public static String getString(final String key) {
        try {
            return RESOURCE_BUNDLE.getString(key);
        } catch (final MissingResourceException e) {
            return '!' + key + '!';
        }
    }
}

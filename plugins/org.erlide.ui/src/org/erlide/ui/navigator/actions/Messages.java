package org.erlide.ui.navigator.actions;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

public final class Messages {
    private static final String BUNDLE_NAME = "org.erlide.ui.navigator.actions.messages"; //$NON-NLS-1$

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
            .getBundle(Messages.BUNDLE_NAME);

    private Messages() {
    }

    public static String getString(final String key) {
        try {
            return Messages.RESOURCE_BUNDLE.getString(key);
        } catch (final MissingResourceException e) {
            return '!' + key + '!';
        }
    }
}

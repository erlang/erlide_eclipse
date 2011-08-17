package org.erlide.tracing.core.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.erlide.tracing.core.Activator;

/**
 * Preferences initializer.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

    public PreferenceInitializer() {
    }

    @Override
    public void initializeDefaultPreferences() {
        final IPreferenceStore store = Activator.getDefault()
                .getPreferenceStore();
        store.setDefault(PreferenceNames.NODE_NAME, "tracing");
        store.setDefault(PreferenceNames.TICK_TIME, 60);
        store.setDefault(PreferenceNames.TRACES_LOAD_LIMIT, 100);
    }
}

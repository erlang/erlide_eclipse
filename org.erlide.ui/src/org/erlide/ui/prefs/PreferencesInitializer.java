/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.ui.prefs;

import static org.erlide.ui.prefs.PreferenceConstants.*;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.graphics.RGB;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.plugin.ColoringPreferencePage;

/**
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class PreferencesInitializer extends AbstractPreferenceInitializer {

    @Override
    public void initializeDefaultPreferences() {
        final String qualifier = ColoringPreferencePage.COLORS_QUALIFIER;
        for (final TokenHighlight ht : TokenHighlight.values()) {
            final IEclipsePreferences node = new DefaultScope()
                    .getNode(qualifier + ht.getName());
            ht.getDefaultData().store(node);
        }

        final IPreferenceStore store = ErlideUIPlugin.getDefault()
                .getPreferenceStore();

        store.setDefault(PRINT_MARGIN, DEFAULT_PRINT_MARGIN);

        store.setDefault(EDITOR_MATCHING_BRACKETS, true);
        PreferenceConverter.setDefault(store, EDITOR_MATCHING_BRACKETS_COLOR,
                new RGB(190, 140, 190));

        // folding
        store.setDefault(EDITOR_FOLDING_ENABLED, true);
        store.setDefault(EDITOR_FOLDING_PROVIDER,
                "org.erlide.ui.editors.defaultFoldingProvider");
        store.setDefault(EDITOR_FOLDING_HEADER_COMMENTS, true);
        store.setDefault(EDITOR_FOLDING_COMMENTS, false);
        store.setDefault(EDITOR_FOLDING_CLAUSES, false);
    }
}

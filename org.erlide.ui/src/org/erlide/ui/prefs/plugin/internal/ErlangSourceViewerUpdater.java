/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.ui.prefs.plugin.internal;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Font;
import org.erlide.ui.editors.erl.ErlangSourceViewerConfiguration;
import org.erlide.ui.prefs.PreferenceConstants;

/**
 * Handles Erlang editor font changes for Erlang source preview viewers.
 *
 */
public class ErlangSourceViewerUpdater {

    /**
     * Creates an Erlang source preview updater for the given viewer,
     * configuration and preference store.
     *
     * @param viewer
     *            the viewer
     * @param configuration
     *            the configuration
     * @param preferenceStore
     *            the preference store
     */
    public ErlangSourceViewerUpdater(final ISourceViewer viewer,
            final ErlangSourceViewerConfiguration configuration,
            final IPreferenceStore preferenceStore) {
        assertThat(viewer, is(not(nullValue())));
        assertThat(configuration, is(not(nullValue())));
        assertThat(preferenceStore, is(not(nullValue())));
        final IPropertyChangeListener fontChangeListener = new IPropertyChangeListener() {
            @Override
            public void propertyChange(final PropertyChangeEvent event) {
                if (PreferenceConstants.EDITOR_TEXT_FONT.equals(event.getProperty())) {
                    final Font font = JFaceResources
                            .getFont(PreferenceConstants.EDITOR_TEXT_FONT);
                    viewer.getTextWidget().setFont(font);
                }
            }
        };
        final IPropertyChangeListener propertyChangeListener = new IPropertyChangeListener() {
            @Override
            public void propertyChange(final PropertyChangeEvent event) {
                if (configuration.affectsTextPresentation(event)) {
                    configuration.handlePropertyChangeEvent(event);
                    viewer.invalidateTextPresentation();
                }
            }
        };
        viewer.getTextWidget().addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(final DisposeEvent e) {
                preferenceStore.removePropertyChangeListener(propertyChangeListener);
                JFaceResources.getFontRegistry().removeListener(fontChangeListener);
            }
        });
        JFaceResources.getFontRegistry().addListener(fontChangeListener);
        preferenceStore.addPropertyChangeListener(propertyChangeListener);
    }
}

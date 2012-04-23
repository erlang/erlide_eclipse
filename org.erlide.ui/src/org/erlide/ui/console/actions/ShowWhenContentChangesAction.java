/*******************************************************************************
 * Copyright (c) 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.console.actions;

import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

/**
 * Abstract action for toggling preference to automatically show the console
 * when a streams content changes.
 * 
 * @since 3.3
 */
public abstract class ShowWhenContentChangesAction extends Action implements
        IPropertyChangeListener {

    /**
     * Constructs an action to toggle console auto activation preferences
     */
    public ShowWhenContentChangesAction(final String name) {
        super(name, IAction.AS_CHECK_BOX);
        setToolTipText(name);
        getPreferenceStore().addPropertyChangeListener(this);
        update();
    }

    @Override
    public void propertyChange(final PropertyChangeEvent event) {
        final String property = event.getProperty();
        if (property.equals(getKey())) {
            update();
        }
    }

    protected abstract String getKey();

    private void update() {
        final IPreferenceStore store = getPreferenceStore();
        if (store.getBoolean(getKey())) {
            // on
            setChecked(true);
        } else {
            // off
            setChecked(false);
        }
    }

    /**
     * @return
     */
    private IPreferenceStore getPreferenceStore() {
        return DebugUITools.getPreferenceStore();
    }

    @Override
    public void run() {
        final IPreferenceStore store = getPreferenceStore();
        final boolean show = isChecked();
        store.removePropertyChangeListener(this);
        store.setValue(getKey(), show);
        store.addPropertyChangeListener(this);
    }

    /**
     * Must be called to dispose this action.
     */
    public void dispose() {
        getPreferenceStore().removePropertyChangeListener(this);
    }

}

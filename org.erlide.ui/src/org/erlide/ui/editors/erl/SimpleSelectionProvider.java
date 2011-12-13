/*******************************************************************************
 * Copyright (c) 2008 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.erlide.ui.actions.SelectionDispatchAction;

/**
 * A simple default implementation of a {@link ISelectionProvider}. It stores
 * the selection and notifies all selection change listeners when the selection
 * is set.
 * 
 * Instances of this class can be used as special selection provider for
 * {@link SelectionDispatchAction}s
 * 
 */
public class SimpleSelectionProvider implements ISelectionProvider {

    private final ListenerList fSelectionChangedListeners;
    private ISelection fSelection;

    /**
     * Create a new SimpleSelectionProvider
     */
    public SimpleSelectionProvider() {
        fSelectionChangedListeners = new ListenerList();
    }

    @Override
    public ISelection getSelection() {
        return fSelection;
    }

    @Override
    public void setSelection(final ISelection selection) {
        fSelection = selection;

        final Object[] listeners = fSelectionChangedListeners.getListeners();
        for (int i = 0; i < listeners.length; i++) {
            ((ISelectionChangedListener) listeners[i])
                    .selectionChanged(new SelectionChangedEvent(this, selection));
        }
    }

    @Override
    public void removeSelectionChangedListener(
            final ISelectionChangedListener listener) {
        fSelectionChangedListeners.remove(listener);
    }

    @Override
    public void addSelectionChangedListener(
            final ISelectionChangedListener listener) {
        fSelectionChangedListeners.add(listener);
    }
}

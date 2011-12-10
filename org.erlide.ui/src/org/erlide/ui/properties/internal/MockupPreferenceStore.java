/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.ui.properties.internal;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

/**
 * Mockup preference store, for registering listeners and firing events, without
 * being an actual store.
 * <p>
 * All methods except firing, adding and removing listeners throw an
 * {@link java.lang.UnsupportedOperationException}.
 * </p>
 * 
 * @since 3.0
 */
public class MockupPreferenceStore implements IPreferenceStore {

    /** Listeners on this store */
    private final ListenerList fListeners = new ListenerList();

    /**
     * {@inheritDoc}
     */
    @Override
    public void addPropertyChangeListener(final IPropertyChangeListener listener) {
        fListeners.add(listener);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removePropertyChangeListener(
            final IPropertyChangeListener listener) {
        fListeners.remove(listener);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void firePropertyChangeEvent(final String name,
            final Object oldValue, final Object newValue) {
        firePropertyChangeEvent(this, name, oldValue, newValue);
    }

    /**
     * Fires a property change event with the given source, property name, old
     * and new value. Used when the event source should be different from this
     * mockup preference store.
     * 
     * @param source
     *            The event source
     * @param name
     *            The property name
     * @param oldValue
     *            The property's old value
     * @param newValue
     *            The property's new value
     */
    public void firePropertyChangeEvent(final Object source, final String name,
            final Object oldValue, final Object newValue) {
        final PropertyChangeEvent event = new PropertyChangeEvent(source, name,
                oldValue, newValue);
        final Object[] listeners = fListeners.getListeners();
        for (final Object element : listeners) {
            ((IPropertyChangeListener) element).propertyChange(event);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean getBoolean(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean getDefaultBoolean(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getDefaultDouble(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public float getDefaultFloat(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getDefaultInt(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getDefaultLong(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDefaultString(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getDouble(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public float getFloat(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getInt(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getLong(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getString(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isDefault(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean needsSaving() {
        return true;
        // throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void putValue(final String name, final String value) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDefault(final String name, final double value) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDefault(final String name, final float value) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDefault(final String name, final int value) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDefault(final String name, final long value) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDefault(final String name, final String defaultObject) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDefault(final String name, final boolean value) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setToDefault(final String name) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setValue(final String name, final double value) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setValue(final String name, final float value) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setValue(final String name, final int value) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setValue(final String name, final long value) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setValue(final String name, final String value) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setValue(final String name, final boolean value) {
        throw new UnsupportedOperationException();
    }

}

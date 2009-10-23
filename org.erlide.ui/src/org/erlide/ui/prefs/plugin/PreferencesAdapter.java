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

package org.erlide.ui.prefs.plugin;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.Preferences;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

/**
 * Adapts {@link org.eclipse.core.runtime.Preferences} to
 * {@link org.eclipse.jface.preference.IPreferenceStore}
 * 
 * @since 3.0
 */
public class PreferencesAdapter implements IPreferenceStore {

	/**
	 * Property change listener. Listens for events of type
	 * {@link org.eclipse.core.runtime.Preferences.PropertyChangeEvent} and
	 * fires a {@link org.eclipse.jface.util.PropertyChangeEvent} on the adapter
	 * with arguments from the received event.
	 */
	class PropertyChangeListener implements Preferences.IPropertyChangeListener {

		/*
		 * @seeorg.eclipse.core.runtime.Preferences.IPropertyChangeListener#
		 * propertyChange
		 * (org.eclipse.core.runtime.Preferences.PropertyChangeEvent)
		 */
		public void propertyChange(final Preferences.PropertyChangeEvent event) {
			firePropertyChangeEvent(event.getProperty(), event.getOldValue(),
					event.getNewValue());
		}
	}

	/** Listeners on the adapter */
	private final ListenerList fListeners = new ListenerList();

	/** Listener on the adapted Preferences */
	private final PropertyChangeListener fListener = new PropertyChangeListener();

	/** Adapted Preferences */
	private final Preferences fPreferences;

	/** True iff no events should be forwarded */
	private boolean fSilent;

	/**
	 * Initialize with empty Preferences.
	 */
	public PreferencesAdapter() {
		this(new Preferences());
	}

	/**
	 * Initialize with the given Preferences.
	 * 
	 * @param preferences
	 *            The preferences to wrap.
	 */
	public PreferencesAdapter(final Preferences preferences) {
		fPreferences = preferences;
	}

	/**
	 * {@inheritDoc}
	 */
	public void addPropertyChangeListener(final IPropertyChangeListener listener) {
		if (fListeners.size() == 0) {
			fPreferences.addPropertyChangeListener(fListener);
		}
		fListeners.add(listener);
	}

	/**
	 * {@inheritDoc}
	 */
	public void removePropertyChangeListener(
			final IPropertyChangeListener listener) {
		fListeners.remove(listener);
		if (fListeners.size() == 0) {
			fPreferences.removePropertyChangeListener(fListener);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean contains(final String name) {
		return fPreferences.contains(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public void firePropertyChangeEvent(final String name,
			final Object oldValue, final Object newValue) {
		if (!fSilent) {
			final PropertyChangeEvent event = new PropertyChangeEvent(this,
					name, oldValue, newValue);
			final Object[] listeners = fListeners.getListeners();
			for (final Object element : listeners) {
				((IPropertyChangeListener) element).propertyChange(event);
			}
		}
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean getBoolean(final String name) {
		return fPreferences.getBoolean(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean getDefaultBoolean(final String name) {
		return fPreferences.getDefaultBoolean(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public double getDefaultDouble(final String name) {
		return fPreferences.getDefaultDouble(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public float getDefaultFloat(final String name) {
		return fPreferences.getDefaultFloat(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public int getDefaultInt(final String name) {
		return fPreferences.getDefaultInt(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public long getDefaultLong(final String name) {
		return fPreferences.getDefaultLong(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public String getDefaultString(final String name) {
		return fPreferences.getDefaultString(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public double getDouble(final String name) {
		return fPreferences.getDouble(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public float getFloat(final String name) {
		return fPreferences.getFloat(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public int getInt(final String name) {
		return fPreferences.getInt(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public long getLong(final String name) {
		return fPreferences.getLong(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public String getString(final String name) {
		return fPreferences.getString(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean isDefault(final String name) {
		return fPreferences.isDefault(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean needsSaving() {
		return fPreferences.needsSaving();
	}

	/**
	 * {@inheritDoc}
	 */
	public void putValue(final String name, final String value) {
		try {
			fSilent = true;
			fPreferences.setValue(name, value);
		} finally {
			fSilent = false;
		}
	}

	/**
	 * {@inheritDoc}
	 */
	public void setDefault(final String name, final double value) {
		fPreferences.setDefault(name, value);
	}

	/**
	 * {@inheritDoc}
	 */
	public void setDefault(final String name, final float value) {
		fPreferences.setDefault(name, value);
	}

	/**
	 * {@inheritDoc}
	 */
	public void setDefault(final String name, final int value) {
		fPreferences.setDefault(name, value);
	}

	/**
	 * {@inheritDoc}
	 */
	public void setDefault(final String name, final long value) {
		fPreferences.setDefault(name, value);
	}

	/**
	 * {@inheritDoc}
	 */
	public void setDefault(final String name, final String defaultObject) {
		fPreferences.setDefault(name, defaultObject);
	}

	/**
	 * {@inheritDoc}
	 */
	public void setDefault(final String name, final boolean value) {
		fPreferences.setDefault(name, value);
	}

	/**
	 * {@inheritDoc}
	 */
	public void setToDefault(final String name) {
		fPreferences.setToDefault(name);
	}

	/**
	 * {@inheritDoc}
	 */
	public void setValue(final String name, final double value) {
		fPreferences.setValue(name, value);
	}

	/**
	 * {@inheritDoc}
	 */
	public void setValue(final String name, final float value) {
		fPreferences.setValue(name, value);
	}

	/**
	 * {@inheritDoc}
	 */
	public void setValue(final String name, final int value) {
		fPreferences.setValue(name, value);
	}

	/**
	 * {@inheritDoc}
	 */
	public void setValue(final String name, final long value) {
		fPreferences.setValue(name, value);
	}

	/**
	 * {@inheritDoc}
	 */
	public void setValue(final String name, final String value) {
		fPreferences.setValue(name, value);
	}

	/**
	 * {@inheritDoc}
	 */
	public void setValue(final String name, final boolean value) {
		fPreferences.setValue(name, value);
	}
}

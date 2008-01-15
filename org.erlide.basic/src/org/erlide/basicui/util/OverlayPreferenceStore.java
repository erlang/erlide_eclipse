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
package org.erlide.basicui.util;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

/**
 * An overlaying preference store.
 * 
 * 
 */
public class OverlayPreferenceStore implements IPreferenceStore {

	/**
	 * Descriptor used to denote data types.
	 */
	public static final class TypeDescriptor {

		public static final TypeDescriptor BOOLEAN = new TypeDescriptor();

		public static final TypeDescriptor DOUBLE = new TypeDescriptor();

		public static final TypeDescriptor FLOAT = new TypeDescriptor();

		public static final TypeDescriptor INT = new TypeDescriptor();

		public static final TypeDescriptor LONG = new TypeDescriptor();

		public static final TypeDescriptor STRING = new TypeDescriptor();

		private TypeDescriptor() {
		}
	}

	/**
	 * Data structure for the overlay key.
	 */
	public static class OverlayKey {

		TypeDescriptor fDescriptor;

		String fKey;

		public OverlayKey(TypeDescriptor descriptor, String key) {
			fDescriptor = descriptor;
			fKey = key;
		}
	}

	/** The parent preference store. */
	protected IPreferenceStore fParent;

	/** This store. */
	protected IPreferenceStore fStore;

	/** The keys of this store. */
	private OverlayKey[] fOverlayKeys;

	/** The property listener. */
	private IPropertyChangeListener fPropertyListener;

	private boolean fLoaded;

	/**
	 * Creates and returns a new overlay preference store.
	 * 
	 * @param parent
	 *            the parent preference store
	 * @param overlayKeys
	 *            the overlay keys
	 */
	public OverlayPreferenceStore(IPreferenceStore parent,
			OverlayKey[] overlayKeys) {
		fParent = parent;
		fOverlayKeys = overlayKeys;
		fStore = new PreferenceStore();
	}

	/**
	 * Tries to find and return the overlay key for the given preference key
	 * string.
	 * 
	 * @param key
	 *            the preference key string
	 * @return the overlay key or <code>null</code> if none can be found
	 */
	OverlayKey findOverlayKey(String key) {
		for (OverlayKey element : fOverlayKeys) {
			if (element.fKey.equals(key)) {
				return element;
			}
		}
		return null;
	}

	/**
	 * Tells whether the given preference key string is covered by this overlay
	 * store.
	 * 
	 * @param key
	 *            the preference key string
	 * @return <code>true</code> if this overlay store covers the given key
	 */
	private boolean covers(String key) {
		return findOverlayKey(key) != null;
	}

	/**
	 * Propagates the given overlay key from the orgin to the target preference
	 * store.
	 * 
	 * @param orgin
	 *            the source preference store
	 * @param key
	 *            the overlay key
	 * @param target
	 *            the preference store to which the key is propagated
	 */
	protected void propagateProperty(IPreferenceStore orgin, OverlayKey key,
			IPreferenceStore target) {

		if (orgin.isDefault(key.fKey)) {
			if (!target.isDefault(key.fKey)) {
				target.setToDefault(key.fKey);
			}
			return;
		}

		final TypeDescriptor d = key.fDescriptor;
		if (TypeDescriptor.BOOLEAN == d) {

			final boolean originValue = orgin.getBoolean(key.fKey);
			final boolean targetValue = target.getBoolean(key.fKey);
			if (targetValue != originValue) {
				target.setValue(key.fKey, originValue);
			}

		} else if (TypeDescriptor.DOUBLE == d) {

			final double originValue = orgin.getDouble(key.fKey);
			final double targetValue = target.getDouble(key.fKey);
			if (targetValue != originValue) {
				target.setValue(key.fKey, originValue);
			}

		} else if (TypeDescriptor.FLOAT == d) {

			final float originValue = orgin.getFloat(key.fKey);
			final float targetValue = target.getFloat(key.fKey);
			if (targetValue != originValue) {
				target.setValue(key.fKey, originValue);
			}

		} else if (TypeDescriptor.INT == d) {

			final int originValue = orgin.getInt(key.fKey);
			final int targetValue = target.getInt(key.fKey);
			if (targetValue != originValue) {
				target.setValue(key.fKey, originValue);
			}

		} else if (TypeDescriptor.LONG == d) {

			final long originValue = orgin.getLong(key.fKey);
			final long targetValue = target.getLong(key.fKey);
			if (targetValue != originValue) {
				target.setValue(key.fKey, originValue);
			}

		} else if (TypeDescriptor.STRING == d) {

			final String originValue = orgin.getString(key.fKey);
			final String targetValue = target.getString(key.fKey);
			if (targetValue != null && originValue != null
					&& !targetValue.equals(originValue)) {
				target.setValue(key.fKey, originValue);
			}

		}
	}

	/**
	 * Propagates all overlay keys from this store to the parent store.
	 */
	public void propagate() {
		for (OverlayKey element : fOverlayKeys) {
			propagateProperty(fStore, element, fParent);
		}
	}

	/**
	 * Loads the given key from the orgin into the target.
	 * 
	 * @param orgin
	 *            the source preference store
	 * @param key
	 *            the overlay key
	 * @param target
	 *            the preference store to which the key is propagated
	 * @param forceInitialization
	 *            if <code>true</code> the value in the target gets
	 *            initialized before loading
	 */
	private void loadProperty(IPreferenceStore orgin, OverlayKey key,
			IPreferenceStore target, boolean forceInitialization) {
		final TypeDescriptor d = key.fDescriptor;
		if (TypeDescriptor.BOOLEAN == d) {

			if (forceInitialization) {
				target.setValue(key.fKey, true);
			}
			target.setValue(key.fKey, orgin.getBoolean(key.fKey));
			target.setDefault(key.fKey, orgin.getDefaultBoolean(key.fKey));

		} else if (TypeDescriptor.DOUBLE == d) {

			if (forceInitialization) {
				target.setValue(key.fKey, 1.0D);
			}
			target.setValue(key.fKey, orgin.getDouble(key.fKey));
			target.setDefault(key.fKey, orgin.getDefaultDouble(key.fKey));

		} else if (TypeDescriptor.FLOAT == d) {

			if (forceInitialization) {
				target.setValue(key.fKey, 1.0F);
			}
			target.setValue(key.fKey, orgin.getFloat(key.fKey));
			target.setDefault(key.fKey, orgin.getDefaultFloat(key.fKey));

		} else if (TypeDescriptor.INT == d) {

			if (forceInitialization) {
				target.setValue(key.fKey, 1);
			}
			target.setValue(key.fKey, orgin.getInt(key.fKey));
			target.setDefault(key.fKey, orgin.getDefaultInt(key.fKey));

		} else if (TypeDescriptor.LONG == d) {

			if (forceInitialization) {
				target.setValue(key.fKey, 1L);
			}
			target.setValue(key.fKey, orgin.getLong(key.fKey));
			target.setDefault(key.fKey, orgin.getDefaultLong(key.fKey));

		} else if (TypeDescriptor.STRING == d) {

			if (forceInitialization) {
				target.setValue(key.fKey, "1"); //$NON-NLS-1$
			}
			target.setValue(key.fKey, orgin.getString(key.fKey));
			target.setDefault(key.fKey, orgin.getDefaultString(key.fKey));

		}
	}

	/**
	 * Loads the values from the parent into this store.
	 */
	public void load() {
		for (OverlayKey element : fOverlayKeys) {
			loadProperty(fParent, element, fStore, true);
		}

		fLoaded = true;
	}

	/**
	 * Loads the default values.
	 */
	public void loadDefaults() {
		for (OverlayKey element : fOverlayKeys) {
			setToDefault(element.fKey);
		}
	}

	/**
	 * Starts to listen for changes.
	 */
	public void start() {
		if (fPropertyListener == null) {
			fPropertyListener = new IPropertyChangeListener() {
				public void propertyChange(PropertyChangeEvent event) {
					final OverlayKey key = findOverlayKey(event.getProperty());
					if (key != null) {
						propagateProperty(fParent, key, fStore);
					}
				}
			};
			fParent.addPropertyChangeListener(fPropertyListener);
		}
	}

	/**
	 * Stops to listen for changes.
	 */
	public void stop() {
		if (fPropertyListener != null) {
			fParent.removePropertyChangeListener(fPropertyListener);
			fPropertyListener = null;
		}
	}

	/*
	 * @see IPreferenceStore#addPropertyChangeListener(org.eclipse.jface.util.IPropertyChangeListener)
	 */
	public void addPropertyChangeListener(IPropertyChangeListener listener) {
		fStore.addPropertyChangeListener(listener);
	}

	/*
	 * @see IPreferenceStore#removePropertyChangeListener(org.eclipse.jface.util.IPropertyChangeListener)
	 */
	public void removePropertyChangeListener(IPropertyChangeListener listener) {
		fStore.removePropertyChangeListener(listener);
	}

	/*
	 * @see IPreferenceStore#firePropertyChangeEvent(java.lang.String,
	 *      java.lang.Object, java.lang.Object)
	 */
	public void firePropertyChangeEvent(String name, Object oldValue,
			Object newValue) {
		fStore.firePropertyChangeEvent(name, oldValue, newValue);
	}

	/*
	 * @see IPreferenceStore#contains(java.lang.String)
	 */
	public boolean contains(String name) {
		return fStore.contains(name);
	}

	/*
	 * @see IPreferenceStore#getBoolean(java.lang.String)
	 */
	public boolean getBoolean(String name) {
		return fStore.getBoolean(name);
	}

	/*
	 * @see IPreferenceStore#getDefaultBoolean(java.lang.String)
	 */
	public boolean getDefaultBoolean(String name) {
		return fStore.getDefaultBoolean(name);
	}

	/*
	 * @see IPreferenceStore#getDefaultDouble(java.lang.String)
	 */
	public double getDefaultDouble(String name) {
		return fStore.getDefaultDouble(name);
	}

	/*
	 * @see IPreferenceStore#getDefaultFloat(String)
	 */
	public float getDefaultFloat(String name) {
		return fStore.getDefaultFloat(name);
	}

	/*
	 * @see IPreferenceStore#getDefaultInt(String)
	 */
	public int getDefaultInt(String name) {
		return fStore.getDefaultInt(name);
	}

	/*
	 * @see IPreferenceStore#getDefaultLong(String)
	 */
	public long getDefaultLong(String name) {
		return fStore.getDefaultLong(name);
	}

	/*
	 * @see IPreferenceStore#getDefaultString(String)
	 */
	public String getDefaultString(String name) {
		return fStore.getDefaultString(name);
	}

	/*
	 * @see IPreferenceStore#getDouble(String)
	 */
	public double getDouble(String name) {
		return fStore.getDouble(name);
	}

	/*
	 * @see IPreferenceStore#getFloat(String)
	 */
	public float getFloat(String name) {
		return fStore.getFloat(name);
	}

	/*
	 * @see IPreferenceStore#getInt(String)
	 */
	public int getInt(String name) {
		return fStore.getInt(name);
	}

	/*
	 * @see IPreferenceStore#getLong(String)
	 */
	public long getLong(String name) {
		return fStore.getLong(name);
	}

	/*
	 * @see IPreferenceStore#getString(String)
	 */
	public String getString(String name) {
		return fStore.getString(name);
	}

	/*
	 * @see IPreferenceStore#isDefault(String)
	 */
	public boolean isDefault(String name) {
		return fStore.isDefault(name);
	}

	/*
	 * @see IPreferenceStore#needsSaving()
	 */
	public boolean needsSaving() {
		return fStore.needsSaving();
	}

	/*
	 * @see IPreferenceStore#putValue(String, String)
	 */
	public void putValue(String name, String value) {
		if (covers(name)) {
			fStore.putValue(name, value);
		}
	}

	/*
	 * @see IPreferenceStore#setDefault(String, double)
	 */
	public void setDefault(String name, double value) {
		if (covers(name)) {
			fStore.setDefault(name, value);
		}
	}

	/*
	 * @see IPreferenceStore#setDefault(String, float)
	 */
	public void setDefault(String name, float value) {
		if (covers(name)) {
			fStore.setDefault(name, value);
		}
	}

	/*
	 * @see IPreferenceStore#setDefault(String, int)
	 */
	public void setDefault(String name, int value) {
		if (covers(name)) {
			fStore.setDefault(name, value);
		}
	}

	/*
	 * @see IPreferenceStore#setDefault(String, long)
	 */
	public void setDefault(String name, long value) {
		if (covers(name)) {
			fStore.setDefault(name, value);
		}
	}

	/*
	 * @see IPreferenceStore#setDefault(String, String)
	 */
	public void setDefault(String name, String value) {
		if (covers(name)) {
			fStore.setDefault(name, value);
		}
	}

	/*
	 * @see IPreferenceStore#setDefault(String, boolean)
	 */
	public void setDefault(String name, boolean value) {
		if (covers(name)) {
			fStore.setDefault(name, value);
		}
	}

	/*
	 * @see IPreferenceStore#setToDefault(String)
	 */
	public void setToDefault(String name) {
		fStore.setToDefault(name);
	}

	/*
	 * @see IPreferenceStore#setValue(String, double)
	 */
	public void setValue(String name, double value) {
		if (covers(name)) {
			fStore.setValue(name, value);
		}
	}

	/*
	 * @see IPreferenceStore#setValue(String, float)
	 */
	public void setValue(String name, float value) {
		if (covers(name)) {
			fStore.setValue(name, value);
		}
	}

	/*
	 * @see IPreferenceStore#setValue(String, int)
	 */
	public void setValue(String name, int value) {
		if (covers(name)) {
			fStore.setValue(name, value);
		}
	}

	/*
	 * @see IPreferenceStore#setValue(String, long)
	 */
	public void setValue(String name, long value) {
		if (covers(name)) {
			fStore.setValue(name, value);
		}
	}

	/*
	 * @see IPreferenceStore#setValue(String, String)
	 */
	public void setValue(String name, String value) {
		if (covers(name)) {
			fStore.setValue(name, value);
		}
	}

	/*
	 * @see IPreferenceStore#setValue(String, boolean)
	 */
	public void setValue(String name, boolean value) {
		if (covers(name)) {
			fStore.setValue(name, value);
		}
	}

	/**
	 * The keys to add to the list of overlay keys.
	 * <p>
	 * Note: This method must be called before {@link #load()}is called.
	 * </p>
	 * 
	 * @param keys
	 * 
	 */
	public void addKeys(OverlayKey[] keys) {
		final int overlayKeysLength = fOverlayKeys.length;
		final OverlayKey[] result = new OverlayKey[keys.length
				+ overlayKeysLength];

		for (int i = 0, length = overlayKeysLength; i < length; i++) {
			result[i] = fOverlayKeys[i];
		}

		for (int i = 0, length = keys.length; i < length; i++) {
			result[overlayKeysLength + i] = keys[i];
		}

		fOverlayKeys = result;

		if (fLoaded) {
			load();
		}
	}
}

/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.WeakHashMap;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.osgi.service.prefs.BackingStoreException;

public class InfoManager<T extends InfoElement> {
	protected final Map<String, T> fElements = new WeakHashMap<String, T>();
	private final String preferencesKey;
	private final Class<? extends InfoElement> elementClass;
	private final String qualifier;
	private String selectedKey = "";

	public InfoManager(Class<T> elem, String qual, String key) {
		qualifier = qual;
		preferencesKey = key;
		elementClass = elem;
	}

	public Collection<T> getElements() {
		return new ArrayList<T>(fElements.values());
	}

	public void store() {
		IEclipsePreferences root = getRootPreferenceNode();
		String[] children;
		try {
			children = root.childrenNames();
			for (String name : children) {
				root.remove(name);
			}

			for (InfoElement rt : fElements.values()) {
				rt.store(root);
			}
			if (selectedKey != null) {
				root.put("default", selectedKey);
			}
			root.flush();
		} catch (BackingStoreException e) {
			e.printStackTrace();
		}
	}

	@SuppressWarnings("unchecked")
	public void load() {
		fElements.clear();
		IEclipsePreferences root = getRootPreferenceNode();
		selectedKey = root.get("default", null);

		String[] children;
		try {
			children = root.childrenNames();
			for (String name : children) {
				try {
					T rt = (T) elementClass.newInstance();
					rt.load(root.node(name));
					fElements.put(name, rt);
				} catch (InstantiationException e) {
					e.printStackTrace();
				} catch (IllegalAccessException e) {
					e.printStackTrace();
				}
			}
			// if (selectedKey == null && children.length == 1) {
			// selectedKey = children[0];
			// }
		} catch (BackingStoreException e) {
			e.printStackTrace();
		}
	}

	protected IEclipsePreferences getRootPreferenceNode() {
		return new InstanceScope().getNode(qualifier + "/" + preferencesKey);
	}

	public void setElements(Collection<T> elements) {
		fElements.clear();
		for (T rt : elements) {
			fElements.put(rt.getName(), rt);
		}
		store();
	}

	public Collection<String> getElementNames() {
		return fElements.keySet();
	}

	public boolean isDuplicateName(String name) {
		for (InfoElement vm : fElements.values()) {
			if (vm.getName().equals(name)) {
				return true;
			}
		}
		return false;
	}

	public T getElement(String name) {
		return fElements.get(name);
	}

	public void removeElement(String name) {
		fElements.remove(name);
	}

	public String getSelectedKey() {
		return this.selectedKey;
	}

	public void setSelectedKey(String selectedKey) {
		this.selectedKey = selectedKey;
	}

}
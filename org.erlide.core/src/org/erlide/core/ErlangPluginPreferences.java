/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Preferences.IPropertyChangeListener;
import org.eclipse.core.runtime.Preferences.PropertyChangeEvent;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.basiccore.ErtsPreferences;
import org.erlide.runtime.ErlangProjectProperties;
import org.osgi.service.prefs.BackingStoreException;

/**
 * @author Vlad Dumitrescu
 */
public class ErlangPluginPreferences implements IPropertyChangeListener {

	private ErlangProjectProperties defaultProjectPrefs;

	private ErtsPreferences ertsPrefs;

	private boolean fTextHoverEnabled = true;

	private static final IPreferencesService prefsService = Platform
			.getPreferencesService();

	/**
	 * 
	 */
	public ErlangPluginPreferences() {
		defaultProjectPrefs = new ErlangProjectProperties();
		ertsPrefs = new ErtsPreferences();
		try {
			prefsService.getRootNode().node(InstanceScope.SCOPE).node(
					ErlangPlugin.PLUGIN_ID).sync();
		} catch (final BackingStoreException e) {
			e.printStackTrace();
		}
		defaultProjectPrefs.load();
		ertsPrefs.load();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.runtime.Preferences.IPropertyChangeListener#propertyChange(org.eclipse.core.runtime.Preferences.PropertyChangeEvent)
	 */
	public void propertyChange(PropertyChangeEvent event) {
		final String property = event.getProperty();

		System.out.println("modified property :> " + property);
		// defaultProjectPrefs.setProperty(property);
	}

	/**
	 * Updates the underlying plugin preferences to the current state.
	 */
	public void updatePluginPreferences() {
		// prefs.removePropertyChangeListener(this);
		ertsPrefs.store();
		defaultProjectPrefs.store();
		try {
			prefsService.getRootNode().node(InstanceScope.SCOPE).node(
					ErlangPlugin.PLUGIN_ID).flush();
		} catch (final BackingStoreException e) {
			e.printStackTrace();
		}
		// prefs.addPropertyChangeListener(this);
	}

	public ErlangProjectProperties getDefaultProjectPrefs() {
		return defaultProjectPrefs;
	}

	public boolean isTextHoverEnabled() {
		return fTextHoverEnabled;
	}

	public void setTextHoverEnabled(boolean textHoverEnabled) {
		fTextHoverEnabled = textHoverEnabled;
	}

	public ErtsPreferences getErtsPrefs() {
		return ertsPrefs;
	}

}

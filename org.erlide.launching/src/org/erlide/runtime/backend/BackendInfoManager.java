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

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.basiccore.ErlLogger;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.osgi.service.prefs.BackingStoreException;

public class BackendInfoManager extends InfoManager<BackendInfo> {

	private static final String BACKENDS = "backends";
	private static BackendInfoManager manager;
	private BackendInfo erlideBackend;

	private BackendInfoManager() {
		super(BackendInfo.class, ErlangLaunchPlugin.PLUGIN_ID, BACKENDS);
		load();
	}

	public static BackendInfoManager getDefault() {
		if (manager == null) {
			manager = new BackendInfoManager();
		}
		return manager;
	}

	public void setErlideBackend(BackendInfo erlideBackend) {
		this.erlideBackend = erlideBackend;
		if (erlideBackend != null) {
			erlideBackend.setErlide(true);
		}
	}

	public BackendInfo getErlideBackend() {
		return this.erlideBackend;
	}

	public BackendInfo getDefaultBackend() {
		return getElement(getSelectedKey());
	}

	@Override
	public void load() {
		super.load();
		IEclipsePreferences root = getRootPreferenceNode();
		setErlideBackend(getElement(root.get("erlide", null)));
	}

	@Override
	public void store() {
		super.store();
		IEclipsePreferences root = getRootPreferenceNode();
		if (erlideBackend != null) {
			root.put("erlide", erlideBackend.getName());
		}
		try {
			root.flush();
		} catch (BackingStoreException e) {
		}
	}

	public void createDefaultBackends(InstallationInfo rt) {
		if (hasBackendsWithRuntime(rt)) {
			return;
		}
		ErlLogger.debug("creating default backends for runtime %s", rt
				.getName());

		BackendInfo result = new BackendInfo();
		result.setInstallation(rt.getName());
		rt.getBackends().add(result);
		result.setName(rt.getName());
		result.setNodeName("n" + rt.getName());
		fElements.put(result.getName(), result);

		String ver = rt.getVersion();
		if (ver != null && ver.compareTo("R12B") >= 0) {
			result = new BackendInfo();
			result.setInstallation(rt.getName());
			rt.getBackends().add(result);
			result.setName(rt.getName() + " SMP");
			result.setArgs("+S 2");
			result.setNodeName("n" + rt.getName() + "smp");
			fElements.put(result.getName(), result);
		}
	}

	private boolean hasBackendsWithRuntime(InstallationInfo rt) {
		for (BackendInfo bi : fElements.values()) {
			if (bi.getInstallation().equals(rt.getName())) {
				return true;
			}
		}
		return false;
	}
}

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
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.osgi.service.prefs.BackingStoreException;

public class RuntimeInfoManager extends InfoManager<RuntimeInfo> {

	private static final String RUNTIMES = "runtimes";
	private static RuntimeInfoManager manager;
	private RuntimeInfo erlideRuntime;

	private RuntimeInfoManager() {
		super(RuntimeInfo.class, ErlangLaunchPlugin.PLUGIN_ID, RUNTIMES);
		load();
	}

	public static RuntimeInfoManager getDefault() {
		if (manager == null) {
			manager = new RuntimeInfoManager();
		}
		return manager;
	}

	public void setErlideRuntime(RuntimeInfo runtime) {
		this.erlideRuntime = runtime;
		if (runtime != null) {
			runtime.setErlide(true);
		}
	}

	public RuntimeInfo getErlideRuntime() {
		return this.erlideRuntime;
	}

	public RuntimeInfo getDefaultRuntime() {
		return getElement(getSelectedKey());
	}

	@Override
	public void load() {
		super.load();
		IEclipsePreferences root = getRootPreferenceNode();
		setErlideRuntime(getElement(root.get("erlide", null)));
	}

	@Override
	public void store() {
		super.store();
		IEclipsePreferences root = getRootPreferenceNode();
		if (erlideRuntime != null) {
			root.put("erlide", erlideRuntime.getName());
		}
		try {
			root.flush();
		} catch (BackingStoreException e) {
		}
	}

	public void createDefaultRuntimes(InstallationInfo rt) {
		if (hasRuntimesWithInstallation(rt)) {
			return;
		}
		ErlLogger.debug("creating default runtimes for installation '%s'", rt
				.getName());

		RuntimeInfo result = new RuntimeInfo();
		result.setInstallation(rt.getName());
		rt.getRuntimes().add(result);
		result.setName(rt.getName());
		result.setNodeName(rt.getName());
		fElements.put(result.getName(), result);

		String ver = rt.getVersion();
		if (ver != null && ver.compareTo("R12B") >= 0) {
			result = new RuntimeInfo();
			result.setInstallation(rt.getName());
			rt.getRuntimes().add(result);
			result.setName(rt.getName() + " SMP");
			result.setArgs("+S 2");
			result.setNodeName(rt.getName() + "-smp");
			fElements.put(result.getName(), result);
		}
	}

	private boolean hasRuntimesWithInstallation(InstallationInfo rt) {
		for (RuntimeInfo bi : fElements.values()) {
			if (bi.getInstallation().equals(rt.getName())) {
				return true;
			}
		}
		return false;
	}
}

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
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.osgi.service.prefs.BackingStoreException;

public class InstallationInfoManager extends InfoManager<InstallationInfo> {
	private static final String OLD_NAME = "erts";
	private static final String INSTALLATIONS = "installations";

	private static InstallationInfoManager manager;

	private InstallationInfoManager() {
		super(InstallationInfo.class, ErlangLaunchPlugin.PLUGIN_ID,
				INSTALLATIONS);
		load();
	}

	public static InstallationInfoManager getDefault() {
		if (manager == null) {
			manager = new InstallationInfoManager();
		}
		return manager;
	}

	public String getDefaultInstallationName() {
		return getSelectedKey();
	}

	public InstallationInfo getDefaultInstallation() {
		return getElement(getSelectedKey());
	}

	@Override
	public void load() {
		super.load();
		IEclipsePreferences old = new InstanceScope()
				.getNode("org.erlide.basic/");
		String oldVal = old.get("otp_home", null);
		if (oldVal != null) {
			ErlLogger.debug("** converting old workspace Erlang settings");

			InstallationInfo rt = new InstallationInfo();
			rt.setOtpHome(oldVal);
			rt.setName(OLD_NAME);
			addElement(rt);
			RuntimeInfoManager.getDefault().createDefaultRuntimes(rt);
			RuntimeInfoManager.getDefault().setSelectedKey(OLD_NAME);
			RuntimeInfoManager.getDefault().setErlideRuntime(
					RuntimeInfoManager.getDefault().getDefaultRuntime());
			old.remove("otp_home");
			try {
				old.flush();
			} catch (BackingStoreException e) {
				e.printStackTrace();
			}
			store();
			RuntimeInfoManager.getDefault().store();
		}

	}
}

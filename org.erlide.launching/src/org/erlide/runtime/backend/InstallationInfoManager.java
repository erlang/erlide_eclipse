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

import org.erlide.basicui.ErlideBasicUIPlugin;

public class InstallationInfoManager extends InfoManager<InstallationInfo> {
	// TODO
	static final String INSTALLATIONS = "installations";

	private static InstallationInfoManager manager;

	private InstallationInfoManager() {
		super(InstallationInfo.class, ErlideBasicUIPlugin.PLUGIN_ID,
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

}

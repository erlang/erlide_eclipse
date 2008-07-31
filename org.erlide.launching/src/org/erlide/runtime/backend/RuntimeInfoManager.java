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

public class RuntimeInfoManager extends InfoManager<RuntimeInfo> {
	static final String RUNTIMES = "runtimes";

	private static RuntimeInfoManager manager;

	private RuntimeInfoManager() {
		super(RuntimeInfo.class, ErlideBasicUIPlugin.PLUGIN_ID, RUNTIMES);
		load();
	}

	public static RuntimeInfoManager getDefault() {
		if (manager == null) {
			manager = new RuntimeInfoManager();
		}
		return manager;
	}

	public String getDefaultRuntimeName() {
		return getSelectedKey();
	}

	public RuntimeInfo getDefaultRuntime() {
		return getElement(getSelectedKey());
	}

}

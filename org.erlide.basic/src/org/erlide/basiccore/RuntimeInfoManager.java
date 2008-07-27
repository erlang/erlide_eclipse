package org.erlide.basiccore;

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

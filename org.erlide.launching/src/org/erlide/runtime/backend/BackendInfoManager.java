package org.erlide.runtime.backend;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.basiccore.InfoManager;
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
}

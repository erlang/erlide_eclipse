package org.erlide.core.preferences;

import org.eclipse.core.runtime.preferences.IScopeContext;

public class PreferenceObject {

	private IScopeContext[] loadContexts;
	private IScopeContext storeContext;
	private ExtendedPreferencesService service;
	private String qualifier;

	PreferenceObject(String qualifier, IScopeContext[] loadContexts,
			IScopeContext storeContext) {
		this.loadContexts = loadContexts;
		this.storeContext = storeContext;
		service = new ExtendedPreferencesService();
		this.qualifier = qualifier;
	}

	public boolean getBoolean(String key, boolean defaultValue) {
		return service.getBoolean(qualifier, key, defaultValue, loadContexts);
	}

	public byte[] getByteArray(String key, byte[] defaultValue) {
		return service.getByteArray(qualifier, key, defaultValue, loadContexts);
	}

	public double getDouble(String key, double defaultValue) {
		return service.getDouble(qualifier, key, defaultValue, loadContexts);
	}

	public float getFloat(String key, float defaultValue) {
		return service.getFloat(qualifier, key, defaultValue, loadContexts);
	}

	public int getInt(String key, int defaultValue) {
		return service.getInt(qualifier, key, defaultValue, loadContexts);
	}

	public long getLong(String key, long defaultValue) {
		return service.getLong(qualifier, key, defaultValue, loadContexts);
	}

	public String getString(String key, String defaultValue) {
		return service.getString(qualifier, key, defaultValue, loadContexts);
	}

	public Object[] getList(String key, Object[] defaultValue) {
		return service.getList(qualifier, key, defaultValue, loadContexts);
	}

}

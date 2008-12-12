package org.erlide.core.preferences;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.osgi.service.prefs.BackingStoreException;

public class PreferencesHelper {

	private IScopeContext[] loadContexts;
	private IScopeContext storeContext;
	private IPreferencesService service;
	private String qualifier;
	private IScopeContext[] nextContexts;

	public PreferencesHelper(String qualifier, IScopeContext[] loadContexts,
			IScopeContext storeContext) {
		this.loadContexts = loadContexts;
		this.storeContext = storeContext;
		nextContexts = getNextContexts(loadContexts, storeContext);
		service = Platform.getPreferencesService();
		this.qualifier = qualifier;
	}

	public PreferencesHelper(String qualifier, IProject project,
			IScopeContext storeContext) {
		this(qualifier, new IScopeContext[] { new ProjectScope(project),
				new InstanceScope(), new ConfigurationScope(),
				new DefaultScope() }, storeContext);
	}

	public PreferencesHelper(String qualifier, IProject project) {
		this(qualifier, new IScopeContext[] { new ProjectScope(project),
				new InstanceScope(), new ConfigurationScope(),
				new DefaultScope() }, new ProjectScope(project));
	}

	public PreferencesHelper(String qualifier, IScopeContext storeContext) {
		this(qualifier, new IScopeContext[] { new InstanceScope(),
				new ConfigurationScope(), new DefaultScope() }, storeContext);
	}

	public PreferencesHelper(String qualifier) {
		this(qualifier, new IScopeContext[] { new InstanceScope(),
				new ConfigurationScope(), new DefaultScope() },
				new InstanceScope());
	}

	public boolean getBoolean(String key, boolean defaultValue) {
		return service.getBoolean(qualifier, key, defaultValue, loadContexts);
	}

	public byte[] getByteArray(String key, byte[] defaultValue) {
		byte[] b = service.getByteArray(qualifier, key, defaultValue,
				loadContexts);
		// there's a bug in PreferenceService.getByteArray, it doesn't decode
		return Base64.decode(b);
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

	public void putBoolean(String key, boolean value) {
		String def = service.getString(qualifier, key, null, nextContexts);
		if (def == null || Boolean.getBoolean(def) != value) {
			storeContext.getNode(qualifier).putBoolean(key, value);
		}
	}

	public static IScopeContext[] getNextContexts(IScopeContext[] list,
			IScopeContext item) {
		List<IScopeContext> result = new ArrayList<IScopeContext>();
		boolean found = false;
		for (IScopeContext ctx : list) {
			if (found) {
				result.add(ctx);
			}
			if (ctx.equals(item)) {
				found = true;
			}
		}
		return result.toArray(new IScopeContext[0]);
	}

	public void putByteArray(String key, byte[] value) {
		byte[] def = service.getByteArray(qualifier, key, null, nextContexts);
		if (def == null || !def.equals(value)) {
			storeContext.getNode(qualifier).putByteArray(key, value);
		}
	}

	public void putDouble(String key, double value) {
		double def = service
				.getDouble(qualifier, key, Double.NaN, nextContexts);
		if (def == Double.NaN || def != value) {
			storeContext.getNode(qualifier).putDouble(key, value);
		}
	}

	public void putFloat(String key, float value) {
		float def = service.getFloat(qualifier, key, Float.NaN, nextContexts);
		if (def == Float.NaN || def != value) {
			storeContext.getNode(qualifier).putFloat(key, value);
		}
	}

	public void putInt(String key, int value) {
		int def = service.getInt(qualifier, key, Integer.MIN_VALUE,
				nextContexts);
		if (def == Integer.MIN_VALUE || def != value) {
			storeContext.getNode(qualifier).putInt(key, value);
		}
	}

	public void putLong(String key, long value) {
		long def = service
				.getLong(qualifier, key, Long.MIN_VALUE, nextContexts);
		if (def == Long.MIN_VALUE || def != value) {
			storeContext.getNode(qualifier).putLong(key, value);
		}
	}

	public void putString(String key, String value) {
		String def = service.getString(qualifier, key, null, nextContexts);
		if (def == null || !def.equals(value)) {
			storeContext.getNode(qualifier).put(key, value);
		}
	}

	public void remove(String key, IScopeContext scope) {
		scope.getNode(qualifier).remove(key);
	}

	public void remove(String key) {
		remove(key, storeContext);
	}

	public void removeNode(String key, IScopeContext scope)
			throws BackingStoreException {
		scope.getNode(qualifier + "/" + key).removeNode();
	}

	public void removeNode(String key) throws BackingStoreException {
		remove(key, storeContext);
	}

}

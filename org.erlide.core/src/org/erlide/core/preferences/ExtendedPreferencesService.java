package org.erlide.core.preferences;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.internal.preferences.EclipsePreferences;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IExportedPreferences;
import org.eclipse.core.runtime.preferences.IPreferenceFilter;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.osgi.service.prefs.Preferences;

public class ExtendedPreferencesService implements IPreferencesService {

	IPreferencesService service = Platform.getPreferencesService();

	public IStatus applyPreferences(IExportedPreferences preferences)
			throws CoreException {
		return service.applyPreferences(preferences);
	}

	public void applyPreferences(IEclipsePreferences node,
			IPreferenceFilter[] filters) throws CoreException {
		service.applyPreferences(node, filters);
	}

	public IStatus exportPreferences(IEclipsePreferences node,
			OutputStream output, String[] excludesList) throws CoreException {
		return service.exportPreferences(node, output, excludesList);
	}

	public void exportPreferences(IEclipsePreferences node,
			IPreferenceFilter[] filters, OutputStream output)
			throws CoreException {
		service.exportPreferences(node, filters, output);
	}

	public String get(String key, String defaultValue, Preferences[] nodes) {
		return service.get(key, defaultValue, nodes);
	}

	public boolean getBoolean(String qualifier, String key,
			boolean defaultValue, IScopeContext[] contexts) {
		return service.getBoolean(qualifier, key, defaultValue, contexts);
	}

	public byte[] getByteArray(String qualifier, String key,
			byte[] defaultValue, IScopeContext[] contexts) {
		return service.getByteArray(qualifier, key, defaultValue, contexts);
	}

	public String[] getDefaultLookupOrder(String qualifier, String key) {
		return service.getDefaultLookupOrder(qualifier, key);
	}

	public double getDouble(String qualifier, String key, double defaultValue,
			IScopeContext[] contexts) {
		return service.getDouble(qualifier, key, defaultValue, contexts);
	}

	public float getFloat(String qualifier, String key, float defaultValue,
			IScopeContext[] contexts) {
		return service.getFloat(qualifier, key, defaultValue, contexts);
	}

	public int getInt(String qualifier, String key, int defaultValue,
			IScopeContext[] contexts) {
		return service.getInt(qualifier, key, defaultValue, contexts);
	}

	public long getLong(String qualifier, String key, long defaultValue,
			IScopeContext[] contexts) {
		return service.getLong(qualifier, key, defaultValue, contexts);
	}

	public String[] getLookupOrder(String qualifier, String key) {
		return service.getLookupOrder(qualifier, key);
	}

	public IEclipsePreferences getRootNode() {
		return service.getRootNode();
	}

	public String getString(String qualifier, String key, String defaultValue,
			IScopeContext[] contexts) {
		return service.getString(qualifier, key, defaultValue, contexts);
	}

	public IStatus importPreferences(InputStream input) throws CoreException {
		return service.importPreferences(input);
	}

	public IPreferenceFilter[] matches(IEclipsePreferences node,
			IPreferenceFilter[] filters) throws CoreException {
		return service.matches(node, filters);
	}

	public IExportedPreferences readPreferences(InputStream input)
			throws CoreException {
		return service.readPreferences(input);
	}

	public void setDefaultLookupOrder(String qualifier, String key,
			String[] order) {
		service.setDefaultLookupOrder(qualifier, key, order);
	}

	private Preferences[] getNodes(String qualifier, String key,
			IScopeContext[] contexts) {
		String[] order = getLookupOrder(qualifier, key);
		String childPath = EclipsePreferences.makeRelative(EclipsePreferences
				.decodePath(key)[0]);
		ArrayList<Preferences> result = new ArrayList<Preferences>();
		for (int i = 0; i < order.length; i++) {
			String scopeString = order[i];
			boolean found = false;
			for (int j = 0; contexts != null && j < contexts.length; j++) {
				IScopeContext context = contexts[j];
				if (context != null && context.getName().equals(scopeString)) {
					Preferences node = context.getNode(qualifier);
					if (node != null) {
						found = true;
						if (childPath != null) {
							node = node.node(childPath);
						}
						result.add(node);
					}
				}
			}
			if (!found) {
				Preferences node = service.getRootNode().node(scopeString)
						.node(qualifier);
				if (childPath != null) {
					node = node.node(childPath);
				}
				result.add(node);
			}
			found = false;
		}
		return result.toArray(new Preferences[result.size()]);
	}

	public String[] childrenNames(Preferences[] nodes) {
		List<String> names = new ArrayList<String>();
		if (nodes == null) {
			return names.toArray(new String[0]);
		}
		for (int i = 0; i < nodes.length; i++) {
			Preferences node = nodes[i];
			if (node != null) {
				try {
					String[] myNames = node.childrenNames();
					for (String aName : myNames) {
						if (!names.contains(aName)) {
							names.add(aName);
						}
					}
				} catch (Exception e) {
				}
			}
		}
		return names.toArray(new String[names.size()]);
	}

	public String[] childrenNames(String qualifier, String key,
			IScopeContext[] scopes) {
		return childrenNames(getNodes(qualifier, key, scopes));

	}

	public Object[] getList(String qualifier, String key,
			Object[] defaultValue, IScopeContext[] scopes) {
		String[] childrenNames = childrenNames(qualifier, key, scopes);
		// TODO
		return defaultValue;
	}
}

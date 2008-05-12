package org.erlide.ui.prefs.plugin;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.erlide.ui.ErlideUIPlugin;
import org.osgi.service.prefs.Preferences;

public abstract class ErlidePreferencePage extends PreferencePage {

	public static IEclipsePreferences getPrefsNode() {
		final String qualifier = ErlideUIPlugin.PLUGIN_ID;
		final IScopeContext context = new InstanceScope();
		final IEclipsePreferences eclipsePreferences = context
				.getNode(qualifier);
		return eclipsePreferences;
	}

	@SuppressWarnings("boxing")
	protected static List<String> getPreferences(String key, String[] keys,
			String[] defaults) {
		final List<String> l = new ArrayList<String>(keys.length);
		final Preferences node = getPrefsNode();
		for (int i = 0; i < keys.length; ++i) {
			final String s = node.get(key + "/" + keys[i], //$NON-NLS-1$
					defaults[i]);
			l.add(s);
		}
		return l;
	}

	protected static Map<String, String> getKeysAndPrefs(String key,
			String[] keys, String[] defaults) {
		final List<String> prefs = getPreferences(key, keys, defaults);
		final Map<String, String> m = new TreeMap<String, String>();
		for (int i = 0; i < keys.length; ++i) {
			m.put(keys[i], prefs.get(i));
		}
		return m;
	}

	public ErlidePreferencePage() {
		super();
	}

	public ErlidePreferencePage(String title) {
		super(title);
	}

	public ErlidePreferencePage(String title, ImageDescriptor image) {
		super(title, image);
	}

	@Override
	public boolean performOk() {
		putPreferences();
		return true;
	}

	protected abstract void putPreferences();

}
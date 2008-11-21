package org.erlide.ui.prefs.plugin;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.erlide.ui.ErlideUIPlugin;
import org.osgi.service.prefs.Preferences;

public abstract class ErlidePreferencePage extends PreferencePage {

	protected static List<String> getPreferences(final String key,
			final String[] keys, final String[] defaults) {
		final List<String> l = new ArrayList<String>(keys.length);
		final Preferences node = ErlideUIPlugin.getPrefsNode();
		for (int i = 0; i < keys.length; ++i) {
			final String s = node.get(key + "/" + keys[i], //$NON-NLS-1$
					defaults[i]);
			l.add(s);
		}
		return l;
	}

	protected static Map<String, String> getKeysAndPrefs(final String key,
			final String[] keys, final String[] defaults) {
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

	public ErlidePreferencePage(final String title) {
		super(title);
	}

	public ErlidePreferencePage(final String title, final ImageDescriptor image) {
		super(title, image);
	}

	@Override
	public boolean performOk() {
		putPreferences();
		return true;
	}

	protected abstract void putPreferences();

}
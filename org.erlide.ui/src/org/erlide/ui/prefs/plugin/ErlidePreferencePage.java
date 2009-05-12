package org.erlide.ui.prefs.plugin;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Button;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.ErlideUIPlugin;
import org.osgi.service.prefs.BackingStoreException;
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

	protected static void addKeysAndPrefs(final String key,
			final String[] keys, final String[] defaults,
			final Map<String, String> m) {
		final List<String> prefs = getPreferences(key, keys, defaults);
		for (int i = 0; i < keys.length; ++i) {
			m.put(keys[i], prefs.get(i));
		}
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

	protected void putPreferences(final String key, final String[] keys,
			final List<Button> buttons) {
		final Preferences node = ErlideUIPlugin.getPrefsNode();
		for (int i = 0; i < keys.length; ++i) {
			final boolean b = buttons.get(i).getSelection();
			node.putBoolean(key + "/" + keys[i], b); //$NON-NLS-1$
		}
		try {
			node.flush();
		} catch (final BackingStoreException e) {
			ErlLogger.warn(e);
		}
	}

	protected void setToDefaults(final String[] keys, final String[] defaults,
			final List<Button> buttons) {
		for (int i = 0; i < keys.length; ++i) {
			buttons.get(i).setSelection(!defaults[i].equals("0")); //$NON-NLS-1$
		}
	}

}
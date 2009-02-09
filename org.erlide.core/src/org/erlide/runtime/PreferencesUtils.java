package org.erlide.runtime;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.osgi.service.prefs.BackingStoreException;

public final class PreferencesUtils {

	public static String packList(Collection<String> list) {
		StringBuffer result = new StringBuffer();
		for (String s : list) {
			result.append(s).append(';');
		}
		return result.toString();
	}

	public static List<String> unpackList(String string) {
		String[] v = string.split(";");
		List<String> result = new ArrayList<String>(Arrays.asList(v));
		return result;
	}

	public static void clearAll(IEclipsePreferences root)
			throws BackingStoreException {
		root.clear();
		for (String n : root.childrenNames()) {
			root.node(n).removeNode();
		}
	}

	private PreferencesUtils() {
	}

}

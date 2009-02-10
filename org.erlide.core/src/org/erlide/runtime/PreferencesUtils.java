package org.erlide.runtime;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.osgi.service.prefs.BackingStoreException;

public final class PreferencesUtils {

	private static final String SEP = ";";

	public static String packList(Iterable<String> list) {
		StringBuilder result = new StringBuilder();
		for (String s : list) {
			result.append(s).append(SEP);
		}
		return result.toString();
	}

	public static List<String> unpackList(String string) {
		String[] v = string.split(SEP);
		List<String> result = new ArrayList<String>(Arrays.asList(v));
		return result;
	}

	public static String packArray(final String[] strs) {
		StringBuilder result = new StringBuilder();
		for (String s : strs) {
			result.append(s).append(SEP);
		}
		return result.toString();
	}

	public static String[] unpackArray(final String str) {
		return unpackList(str).toArray(new String[0]);
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

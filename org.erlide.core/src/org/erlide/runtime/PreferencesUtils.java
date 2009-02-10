package org.erlide.runtime;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
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
		return unpackList(string, SEP);
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

	public static List<String> readFile(String file) {
		List<String> res = new ArrayList<String>();
		try {
			BufferedReader reader = new BufferedReader(new FileReader(file));
			String line;
			while ((line = reader.readLine()) != null) {
				res.add(line);
			}
		} catch (IOException e) {
		}
		return res;
	}

	private PreferencesUtils() {
	}

	public static List<String> unpackList(String string, String sep) {
		String[] v = string.split(sep);
		List<String> result = new ArrayList<String>(Arrays.asList(v));
		return result;
	}

}

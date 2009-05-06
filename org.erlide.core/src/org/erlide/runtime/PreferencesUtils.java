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

	public static String packList(final Iterable<String> list) {
		final StringBuilder result = new StringBuilder();
		for (final String s : list) {
			result.append(s).append(SEP);
		}
		return result.toString();
	}

	public static List<String> unpackList(final String string) {
		return unpackList(string, SEP);
	}

	public static String packArray(final String[] strs) {
		final StringBuilder result = new StringBuilder();
		for (final String s : strs) {
			result.append(s).append(SEP);
		}
		return result.toString();
	}

	public static String[] unpackArray(final String str) {
		return unpackList(str).toArray(new String[0]);
	}

	public static void clearAll(final IEclipsePreferences root)
			throws BackingStoreException {
		root.clear();
		for (final String n : root.childrenNames()) {
			root.node(n).removeNode();
		}
	}

	public static List<String> readFile(final String file) {
		final List<String> res = new ArrayList<String>();
		try {
			final BufferedReader reader = new BufferedReader(new FileReader(
					file));
			String line;
			while ((line = reader.readLine()) != null) {
				res.add(line);
			}
		} catch (final IOException e) {
		}
		return res;
	}

	private PreferencesUtils() {
	}

	public static List<String> unpackList(final String string, final String sep) {
		final String[] v = string.split(sep);
		final List<String> result = new ArrayList<String>(Arrays.asList(v));
		return result;
	}

}

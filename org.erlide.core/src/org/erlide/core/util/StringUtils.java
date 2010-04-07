package org.erlide.core.util;

public final class StringUtils {

	private StringUtils() {
	}

	public static String join(final String[] cmds) {
		StringBuilder result = new StringBuilder();
		for (String cmd : cmds) {
			result.append(cmd).append(' ');
		}
		return result.toString();
	}

}

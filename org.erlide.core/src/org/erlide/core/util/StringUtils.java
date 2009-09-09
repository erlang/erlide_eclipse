package org.erlide.core.util;

public final class StringUtils {

	private StringUtils() {
	}

	public static String join(String[] cmds) {
		String result = "";
		for (String cmd : cmds)
			result += cmd + " ";
		return result;
	}

}

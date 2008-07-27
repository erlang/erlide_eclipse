package org.erlide.basicui.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class PreferencesUtils {

	public static String packList(List<String> list) {
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

}

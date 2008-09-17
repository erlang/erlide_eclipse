package org.erlide.wrangler.refactoring.util;

public class NameChecker {

	private NameChecker() {
	}

	public static boolean checkIsAtom(String s) {
		if (s.length() == 0) {
			return false;
		}
		if (s.charAt(0) == '\'' && s.charAt(s.length() - 1) == '\'') {
			return true;
		} else {
			if (s.substring(0, 1).replaceAll("[a-z]", "").length() == 0
					&& s.replaceAll("[A-Za-z_@0-9]", "").length() == 0) {
				return true;
			} else {
				return false;
			}
		}
	}

	public static boolean checkIsVariable(String s) {
		if (s.length() == 0) {
			return false;
		}

		if (s.startsWith("_")
				|| s.substring(0, 1).toUpperCase().equals(s.substring(0, 1))) {
			if (s.replaceAll("[A-Za-z_@0-9]", "").length() == 0) {
				return true;
			} else {
				return false;
			}
		} else {
			return false;
		}
	}

}

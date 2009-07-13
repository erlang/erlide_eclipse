package org.erlide.wrangler.refactoring.ui.validator;

public class AtomValidator implements IValidator {

	public boolean isValid(String s) {
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

}

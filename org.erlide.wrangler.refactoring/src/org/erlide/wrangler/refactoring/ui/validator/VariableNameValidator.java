package org.erlide.wrangler.refactoring.ui.validator;

public class VariableNameValidator implements IValidator {

	public boolean isValid(String s) {
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

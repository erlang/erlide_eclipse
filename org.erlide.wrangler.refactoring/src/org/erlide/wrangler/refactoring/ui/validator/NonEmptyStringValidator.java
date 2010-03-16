package org.erlide.wrangler.refactoring.ui.validator;

public class NonEmptyStringValidator implements IValidator {

	public boolean isValid(String text) {
		return !text.equals("");
	}

}

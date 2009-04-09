package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.util.List;

import org.erlide.wrangler.refactoring.core.RefactoringParameters;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;

import com.ericsson.otp.erlang.OtpErlangObject;

public abstract class AbstractDuplicatesParser implements IResultParser {

	protected String errorMessage;
	protected boolean isSuccessful;
	protected List<DuplicatedCodeElement> duplicates;

	public AbstractDuplicatesParser(OtpErlangObject obj,
			RefactoringParameters parameter) {
		parse(obj, parameter);
	}

	public List<DuplicatedCodeElement> getDuplicates() {
		return duplicates;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public boolean isSuccessful() {
		return isSuccessful;
	}

	protected void setUnSuccessful(String errorMessage) {
		isSuccessful = false;
		this.errorMessage = errorMessage;
	}
}

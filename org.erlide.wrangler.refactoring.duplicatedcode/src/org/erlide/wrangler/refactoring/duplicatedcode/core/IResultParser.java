package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.util.List;

import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface IResultParser {
	public boolean isSuccessful();

	public String getErrorMessage();

	public void parse(OtpErlangObject object);

	public List<DuplicatedCodeElement> getDuplicates();
}

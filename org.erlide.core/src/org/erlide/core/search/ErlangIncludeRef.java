package org.erlide.core.search;

import org.erlide.core.erlang.IErlElement.Kind;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangIncludeRef extends ErlangElementRef {

	private static final OtpErlangAtom INCLUDE_REF_ATOM = new OtpErlangAtom(
			"include_ref");
	private final String filename;

	public ErlangIncludeRef(final String filename) {
		super();
		this.filename = filename;
	}

	public String getFilename() {
		return filename;
	}

	@Override
	public String toString() {
		return "include " + filename;
	}

	@Override
	public OtpErlangObject getSearchObject() {
		return new OtpErlangTuple(new OtpErlangObject[] { INCLUDE_REF_ATOM,
				new OtpErlangString(filename) });
	}

	@Override
	public String searchElementToString(final ErlangSearchElement ese) {
		return ese.getAttribute();
	}

	@Override
	public Kind searchElementToKind(final ErlangSearchElement ese) {
		return Kind.ATTRIBUTE;
	}

}

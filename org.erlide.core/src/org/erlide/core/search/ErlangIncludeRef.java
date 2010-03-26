package org.erlide.core.search;

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

}

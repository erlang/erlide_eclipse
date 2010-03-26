package org.erlide.core.search;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangMacroRef extends ErlangElementRef {

	private static final OtpErlangAtom MACRO_REF_ATOM = new OtpErlangAtom(
			"macro_ref");
	private final String macro;

	public ErlangMacroRef(final String macro) {
		super();
		this.macro = macro;
	}

	public String getMacro() {
		return macro;
	}

	@Override
	public String toString() {
		return "?" + macro;
	}

	@Override
	public OtpErlangObject getSearchObject() {
		return new OtpErlangTuple(new OtpErlangObject[] { MACRO_REF_ATOM,
				new OtpErlangAtom("?" + macro) });
	}

}

package org.erlide.core.search;

import org.erlide.core.erlang.IErlElement.Kind;

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
				new OtpErlangAtom(macro) });
	}

	@Override
	public String searchElementToString(final ErlangSearchElement ese) {
		return "?" + ese.getAttribute();
	}

	@Override
	public Kind searchElementToKind(final ErlangSearchElement ese) {
		return Kind.MACRO_DEF; // FIXME: should we have a macro-ref element?
	}

}

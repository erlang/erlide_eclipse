package org.erlide.core.search;

import org.erlide.core.erlang.IErlElement.Kind;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangMacroDefRef extends ErlangElementRef {

	private static final OtpErlangAtom MACRO_DEF_ATOM = new OtpErlangAtom(
			"macro_def");
	private final String macro;

	public ErlangMacroDefRef(final String macro) {
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
		return new OtpErlangTuple(new OtpErlangObject[] { MACRO_DEF_ATOM,
				new OtpErlangAtom(macro) });
	}

	@Override
	public String searchElementToString(final ErlangSearchElement ese) {
		return "macro_definition: " + ese.getAttribute();
	}

	@Override
	public Kind searchElementToKind(final ErlangSearchElement ese) {
		return Kind.MACRO_DEF;
	}

}

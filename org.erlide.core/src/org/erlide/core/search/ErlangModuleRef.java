package org.erlide.core.search;

import org.erlide.core.erlang.IErlElement.Kind;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangModuleRef extends ErlangElementRef {

	private static final OtpErlangAtom MODULE_ATOM = new OtpErlangAtom("module");
	private final String module;

	public ErlangModuleRef(final String module) {
		super();
		this.module = module;
	}

	public String getModule() {
		return module;
	}

	@Override
	public String toString() {
		return module;
	}

	@Override
	public OtpErlangObject getSearchObject() {
		return new OtpErlangTuple(new OtpErlangObject[] { MODULE_ATOM,
				new OtpErlangAtom(module) });
	}

	@Override
	public String searchElementToString(final ErlangSearchElement ese) {
		return ese.getAttribute();
	}

	@Override
	public Kind searchElementToKind(final ErlangSearchElement ese) {
		return Kind.MODULE;
	}

}

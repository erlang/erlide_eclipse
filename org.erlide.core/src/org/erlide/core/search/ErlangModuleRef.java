package org.erlide.core.search;

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
		return new OtpErlangTuple(new OtpErlangObject[] {
				MODULE_ATOM, new OtpErlangAtom(module) });
	}

}

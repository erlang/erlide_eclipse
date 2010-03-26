package org.erlide.core.search;

import org.erlide.core.erlang.IErlElement.Kind;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangExternalFunctionCallRef extends ErlangElementRef {
	private static final OtpErlangAtom EXTERNAL_CALL_ATOM = new OtpErlangAtom(
			"external_call");
	private final String module;
	private final String function;
	private final int arity;

	public ErlangExternalFunctionCallRef(final String module,
			final String function, final int arity) {
		super();
		this.module = module;
		this.function = function;
		this.arity = arity;
	}

	/**
	 * @return the module
	 */
	public String getModule() {
		return module;
	}

	/**
	 * @return the function
	 */
	public String getFunction() {
		return function;
	}

	/**
	 * @return the arity
	 */
	public int getArity() {
		return arity;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		String s = module + ":" + function;
		if (arity == -1) {
			return s;
		}
		return s + "/" + arity;
	}

	@Override
	public OtpErlangObject getSearchObject() {
		return new OtpErlangTuple(new OtpErlangObject[] { EXTERNAL_CALL_ATOM,
				new OtpErlangAtom(module), new OtpErlangAtom(function),
				new OtpErlangLong(arity) });
	}

	@Override
	public String searchElementToString(final ErlangSearchElement ese) {
		return searchElementFunctionToString(ese);
	}

	@Override
	public Kind searchElementToKind(final ErlangSearchElement ese) {
		return searchElementFunctionToKind(ese);
	}

}

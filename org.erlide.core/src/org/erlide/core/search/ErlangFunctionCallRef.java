package org.erlide.core.search;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangFunctionCallRef extends ErlangElementRef {

	private static final OtpErlangAtom FUNCTION_CALL_ATOM = new OtpErlangAtom(
			"function_call");
	private final String function;
	private final int arity;

	public ErlangFunctionCallRef(final String function, final int arity) {
		super();
		this.function = function;
		this.arity = arity;
	}

	public String getFunction() {
		return function;
	}

	public int getArity() {
		return arity;
	}

	@Override
	public String toString() {
		if (arity == -1) {
			return function;
		}
		return function + "/" + arity;
	}

	@Override
	public OtpErlangObject getSearchObject() {
		return new OtpErlangTuple(new OtpErlangObject[] { FUNCTION_CALL_ATOM,
				new OtpErlangAtom(function), new OtpErlangLong(arity) });
	}

}

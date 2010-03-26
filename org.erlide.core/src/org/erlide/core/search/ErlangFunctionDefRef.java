package org.erlide.core.search;

import org.erlide.core.erlang.IErlElement.Kind;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangFunctionDefRef extends ErlangElementRef {
	private static final OtpErlangAtom FUNCTION_DEF_ATOM = new OtpErlangAtom(
			"function_def");
	private final String function;
	private final int arity;

	public ErlangFunctionDefRef(final String function, final int arity) {
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
		return new OtpErlangTuple(new OtpErlangObject[] { FUNCTION_DEF_ATOM,
				new OtpErlangAtom(function), new OtpErlangLong(arity) });
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

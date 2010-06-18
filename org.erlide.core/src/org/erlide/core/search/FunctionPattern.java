package org.erlide.core.search;

import com.ericsson.otp.erlang.OtpErlangObject;

import erlang.ErlangSearchPattern;

public class FunctionPattern extends ErlangSearchPattern {

	private final String module;
	private final String name;
	private final int arity;

	public FunctionPattern(final String module, final String name,
			final int arity, final int limitTo) {
		super(limitTo);
		this.module = module;
		this.name = name;
		this.arity = arity;
	}

	@Override
	public OtpErlangObject getSearchObject() {
		if (module == null || module.length() == 0) {
			return makeFAPatternObject(FUNCTION_DEF_ATOM, FUNCTION_CALL_ATOM,
					name, arity);
		} else {
			return makeMFAPatternObject(FUNCTION_DEF_ATOM, EXTERNAL_CALL_ATOM,
					module, name, arity);
		}
	}

	@Override
	public String patternString() {
		final String s = name + "/" + arity;
		if (module == null || limitTo != REFERENCES) {
			return s;
		} else {
			return module + ":" + s;
		}
	}

	@Override
	public int getSearchFor() {
		return SEARCHFOR_FUNCTION;
	}

	@Override
	public String labelString() {
		final String s = name + "/" + arity;
		if (module == null || limitTo != REFERENCES) {
			return s;
		} else {
			return module + ":" + s;
		}
	}

}
package org.erlide.core.search;

import com.ericsson.otp.erlang.OtpErlangObject;

import erlang.ErlangSearchPattern;

public class TypeRefPattern extends ErlangSearchPattern {

	private final String module;

	@Override
	public String toString() {
		return "TypeRefPattern [module=" + module + ", name=" + name
				+ ", limitTo=" + limitTo + "]";
	}

	private final String name;

	public TypeRefPattern(final String module, final String name,
			final int limitTo) {
		super(limitTo);
		this.module = module;
		this.name = name;
	}

	@Override
	public OtpErlangObject getSearchObject() {
		return makeSSPatternObject(TYPE_DEF_ATOM, TYPE_REF_ATOM,
				(module == null) ? "_" : module, name);
	}

	@Override
	public String patternString() {
		if (module != null && module.length() != 0) {
			return module + ":" + name;
		}
		return name;
	}

	@Override
	public int getSearchFor() {
		return SEARCHFOR_TYPE;
	}

	@Override
	public String labelString() {
		if (module != null && module.length() != 0) {
			return module + ":" + name;
		}
		return name;
	}

}

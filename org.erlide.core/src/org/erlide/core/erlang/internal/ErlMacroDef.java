package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlMacroDef;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.util.Util;

public class ErlMacroDef extends ErlMember implements IErlMacroDef {

	String macro;
	String extra;

	/**
	 * @param parent
	 * @param imports
	 * @param module
	 */
	protected ErlMacroDef(final IErlElement parent, final String macro,
			final String extra) {
		super(parent, "macro_definition");
		this.macro = macro;
		this.extra = extra;
	}

	public ErlMacroDef(final IErlModule parent, final String extra) {
		super(parent, "macro_definition");
		this.extra = extra;
		macro = uptoCommaOrParen(extra);
	}

	private String uptoCommaOrParen(final String s) {
		if (s == null || s.length() == 0) {
			return s;
		}
		int i = 0;
		if (s.charAt(0) == '\'') {
			i = s.indexOf('\'', 1);
		}
		if (i == -1) {
			i = 0;
		}
		int j = s.indexOf(',', i);
		if (j == 0 || j == -1) {
			j = s.length();
		}
		final int k = s.indexOf('(', i);
		if (k < j && k > 0) {
			j = k;
		}
		return s.substring(0, j);
	}

	public Kind getKind() {
		return Kind.MACRO_DEF;
	}

	public String getDefinedName() {
		return macro;
	}

	@Override
	public String toString() {
		return getName() + ": " + getDefinedName();
	}

	@Override
	public int hashCode() {
		return Util.combineHashCodes(super.hashCode(), getDefinedName()
				.hashCode());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.core.erlang.internal.ErlElement#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object o) {
		if (o instanceof ErlMacroDef) {
			final ErlMacroDef other = (ErlMacroDef) o;
			return super.equals(o) && macro.equals(other.macro);
		}
		return false;
	}

	public String getExtra() {
		return extra;
	}

}

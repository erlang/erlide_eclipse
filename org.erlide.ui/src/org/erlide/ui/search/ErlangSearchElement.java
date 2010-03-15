package org.erlide.ui.search;

import org.erlide.core.erlang.util.ErlangFunction;

public class ErlangSearchElement {

	private final String moduleName;
	private final ErlangFunction function;
	private final String clauseHead;

	ErlangSearchElement(final String moduleName, final ErlangFunction function,
			final String clauseHead) {
		this.moduleName = moduleName;
		this.function = function;
		this.clauseHead = clauseHead;
	}

	public String getModuleName() {
		return moduleName;
	}

	public ErlangFunction getFunction() {
		return function;
	}

	public String getClauseHead() {
		return clauseHead;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof ErlangSearchElement) {
			final ErlangSearchElement e = (ErlangSearchElement) o;
			if (e.moduleName.equals(moduleName) && e.function.equals(function)) {
				if (e.clauseHead == null) {
					return clauseHead == null;
				}
				return e.clauseHead.equals(clauseHead);
			}
		}
		return false;
	}

	@Override
	public int hashCode() {
		final int multiplier = 37; // some prime
		int hashCode = 13; // some random value
		hashCode = hashCode * multiplier + moduleName.hashCode();
		hashCode = hashCode * multiplier + function.hashCode();
		if (clauseHead != null) {
			hashCode = hashCode * multiplier + clauseHead.hashCode();
		}
		return hashCode;
	}

}

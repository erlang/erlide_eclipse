package org.erlide.ui.search;

import org.erlide.core.erlang.util.ErlangFunction;

public class ErlangSearchElement {

	private final String moduleName;
	private final ErlangFunction function;
	private final String arguments;
	private final boolean subClause;

	ErlangSearchElement(final String moduleName, final ErlangFunction function,
			final String arguments, final boolean subClause) {
		this.moduleName = moduleName;
		this.function = function;
		this.arguments = arguments;
		this.subClause = subClause;
	}

	public String getModuleName() {
		return moduleName;
	}

	public ErlangFunction getFunction() {
		return function;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof ErlangSearchElement) {
			final ErlangSearchElement e = (ErlangSearchElement) o;
			if (e.moduleName.equals(moduleName) && e.function.equals(function)
					&& e.subClause == subClause) {
				if (e.arguments == null) {
					return arguments == null;
				}
				return e.arguments.equals(arguments);
			}
		}
		return false;
	}

	@Override
	public int hashCode() {
		final int multiplier = 37; // some prime
		int hashCode = 13; // some random value
		if (subClause) {
			hashCode++;
		}
		hashCode = hashCode * multiplier + moduleName.hashCode();
		hashCode = hashCode * multiplier + function.hashCode();
		if (arguments != null) {
			hashCode = hashCode * multiplier + arguments.hashCode();
		}
		return hashCode;
	}

	public String getArguments() {
		return arguments;
	}

	public boolean isSubClause() {
		return subClause;
	}

}

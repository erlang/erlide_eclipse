package org.erlide.core.search;

import org.erlide.core.erlang.util.ErlangFunction;

public class ErlangSearchElement {

	private final String moduleName;
	private final ErlangFunction function;
	private final String arguments;
	private final boolean subClause;
	private final String attribute;

	public ErlangSearchElement(final String moduleName,
			final ErlangFunction function, final String arguments,
			final boolean subClause, final String attribute) {
		this.moduleName = moduleName;
		this.function = function;
		this.arguments = arguments;
		this.subClause = subClause;
		this.attribute = attribute;
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
			if (e.moduleName.equals(moduleName)) {
				final boolean functionEqual = e.function == null ? function == null
						: e.function.equals(function);
				if (functionEqual && e.subClause == subClause) {
					if (e.arguments == null) {
						return arguments == null;
					}
					return e.arguments.equals(arguments);
				}
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
		if (function != null) {
			hashCode = hashCode * multiplier + function.hashCode();
		}
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

	public String getAttribute() {
		return attribute;
	}

}

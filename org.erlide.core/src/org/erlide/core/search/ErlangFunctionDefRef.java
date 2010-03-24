package org.erlide.core.search;

public class ErlangFunctionDefRef extends ErlangElementRef {
	private final String name;
	private final int arity;

	public ErlangFunctionDefRef(final String name, final int arity) {
		super();
		this.name = name;
		this.arity = arity;
	}

	public String getName() {
		return name;
	}

	public int getArity() {
		return arity;
	}

	@Override
	public String toString() {
		return name + "/" + arity;
	}

}

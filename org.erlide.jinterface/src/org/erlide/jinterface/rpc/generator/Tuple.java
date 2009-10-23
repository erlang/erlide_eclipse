package org.erlide.jinterface.rpc.generator;


public class Tuple extends BaseTuple {

	// Display a comma-separated list of elements.
	public Tuple() {
		super("(", ", ", ")");
	}

	// Add generic elements to the tuple.
	// Supports dot-chaining.
	public Tuple add(final Object o) {
		super.addElement(o);
		return this;
	}

	public Tuple add(final int i) {
		super.addElement(i);
		return this;
	}
}

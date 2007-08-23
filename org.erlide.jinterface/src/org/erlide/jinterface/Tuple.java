package org.erlide.jinterface;

public class Tuple extends BaseTuple {

	// Display a coma-separated list of elements.
	public Tuple() {
		super("(", ", ", ")");
	}

	// Add generic elements to the tuple.
	// Supports dot-chaining.
	public Tuple add(Object o) {
		super.addElement(o);
		return this;
	}

	public Tuple add(int i) {
		super.addElement(i);
		return this;
	}
}

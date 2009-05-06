package org.erlide.jinterface.rpc.generator;

import java.util.ArrayList;
import java.util.Iterator;

public abstract class BaseTuple implements Comparable<BaseTuple> {

	// Ordered collection of elements.
	final ArrayList<Object> elements = new ArrayList<Object>();

	// Strings used to display the tuple.
	final String open;
	final String separator;
	final String close;

	// Initialize the strings for this tuple type.
	protected BaseTuple(final String open, final String separator, final String close) {
		this.open = open;
		this.separator = separator;
		this.close = close;
	}

	// Add elements to the tuple. Supports dot-chaining.
	protected BaseTuple addElement(final Object o) {
		this.elements.add(o);
		return this;
	}

	public Object get(final int i) {
		return this.elements.get(i);
	}

	protected BaseTuple addElement(final int i) {
		return addElement(Integer.valueOf(i));
	}

	// Compare two tuples. All elements must be equal.
	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof BaseTuple)) {
			return false;
		}
		final BaseTuple that = (BaseTuple) obj;
		if (that.elements.size() != this.elements.size()) {
			return false;
		}
		for (int i = 0; i < this.elements.size(); ++i) {
			if (!that.elements.get(i).equals(this.elements.get(i))) {
				return false;
			}
		}
		return true;
	}

	// Calculate a hash code based on the hash of each element.
	@Override
	public int hashCode() {
		int result = 0;
		final Iterator<Object> it = this.elements.iterator();
		while (it.hasNext()) {
			result = result * 37 + it.next().hashCode();
		}
		return result;
	}

	// Display the tuple using the open, separator, and close
	// specified in the constructor.
	@Override
	public String toString() {
		final StringBuilder result = new StringBuilder(this.open);
		final Iterator<Object> it = this.elements.iterator();
		while (it.hasNext()) {
			result.append(it.next());
			if (it.hasNext()) {
				result.append(this.separator);
			}
		}
		return result.append(this.close).toString();
	}

	// Order by the most significant element first.
	// The tuples must agree in size and type.
	@SuppressWarnings("unchecked")
	public int compareTo(final BaseTuple that) {
		for (int i = 0; i < this.elements.size(); ++i) {
			final int compare = ((Comparable<Object>) this.elements.get(i))
			.compareTo(that.elements.get(i));
			if (compare != 0) {
				return compare;
			}
		}
		return 0;
	}
}

package org.erlide.core.search;

public class ErlangIncludeRef extends ErlangElementRef {

	private final String filename;

	public ErlangIncludeRef(final String filename) {
		super();
		this.filename = filename;
	}

	public String getFilename() {
		return filename;
	}

	@Override
	public String toString() {
		return "include " + getFilename();
	}

}

package org.erlide.core.search;

public class ErlangMacroRef extends ErlangElementRef {

	private final String macro;

	public ErlangMacroRef(final String macro) {
		super();
		this.macro = macro;
	}

	public String getMacro() {
		return macro;
	}

	@Override
	public String toString() {
		return "?" + getMacro();
	}

}

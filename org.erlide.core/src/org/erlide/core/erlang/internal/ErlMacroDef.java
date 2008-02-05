package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlMacroDef;
import org.erlide.core.erlang.util.Util;

public class ErlMacroDef extends ErlMember implements IErlMacroDef {

	String macro;

	/**
	 * @param parent
	 * @param imports
	 * @param module
	 */
	protected ErlMacroDef(IErlElement parent, String macro) {
		super(parent, "macro_definition");
		this.macro = macro;
	}

	public ErlElementType getElementType() {
		return ErlElementType.MACRO_DEF;
	}

	public String getDefinedName() {
		return macro;
	}

	@Override
	public String toString() {
		return getElementName() + ": " + getDefinedName();
	}

	@Override
	public int hashCode() {
		return Util.combineHashCodes(super.hashCode(), getDefinedName()
				.hashCode());
	}

}

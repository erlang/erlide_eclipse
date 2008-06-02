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

	public Kind getKind() {
		return Kind.MACRO_DEF;
	}

	public String getDefinedName() {
		return macro;
	}

	@Override
	public String toString() {
		return getName() + ": " + getDefinedName();
	}

	@Override
	public int hashCode() {
		return Util.combineHashCodes(super.hashCode(), getDefinedName()
				.hashCode());
	}

	/* (non-Javadoc)
	 * @see org.erlide.core.erlang.internal.ErlElement#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		if (o instanceof ErlMacroDef) {
			final ErlMacroDef other = (ErlMacroDef) o;
			return super.equals(o) && macro.equals(other.macro); 
		}
		return false;
	}

}

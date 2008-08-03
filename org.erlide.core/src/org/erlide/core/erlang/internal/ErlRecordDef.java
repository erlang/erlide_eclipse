package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlRecordDef;
import org.erlide.core.erlang.util.Util;

public class ErlRecordDef extends ErlMember implements IErlRecordDef {

	String record;
	String extra;

	/**
	 * @param parent
	 * @param imports
	 * @param module
	 */
	protected ErlRecordDef(final IErlElement parent, final String record,
			final String extra) {
		super(parent, "record_definition");
		this.record = record;
		this.extra = extra;
	}

	public Kind getKind() {
		return Kind.RECORD_DEF;
	}

	public String getDefinedName() {
		return record;
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

	@Override
	public boolean equals(final Object o) {
		if (this == o) {
			return true;
		}
		if (o == null) {
			return false;
		}

		// Erlang model parent is null
		if (fParent == null) {
			return super.equals(o);
		}

		if (o instanceof ErlElement) {
			return toString().equals(o.toString());
		}
		return false;
	}

	public String getExtra() {
		return extra;
	}

}

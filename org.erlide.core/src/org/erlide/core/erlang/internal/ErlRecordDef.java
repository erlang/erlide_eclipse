package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlRecordDef;

public class ErlRecordDef extends ErlMember implements IErlRecordDef {

	String record;

	/**
	 * @param parent
	 * @param imports
	 * @param module
	 */
	protected ErlRecordDef(IErlElement parent, String record) {
		super(parent, "record_definition");
		this.record = record;
	}

	public String getElementType() {
		return RECORD_DEF;
	}

	public String getDefinedName() {
		return record;
	}

	@Override
	public String toString() {
		return getElementName() + ": " + getDefinedName();
	}
}

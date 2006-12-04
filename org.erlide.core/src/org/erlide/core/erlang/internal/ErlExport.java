package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlExport;
import org.erlide.core.erlang.IParent;

public class ErlExport extends ErlMember implements IErlExport, IParent {

	protected ErlExport(IErlElement parent) {
		super(parent, "export");
	}

	public String getElementType() {
		return EXPORT;
	}

	@Override
	public String toString() {
		return getElementName();
	}

}

package org.erlide.core.search;

public class ErlangRecordRef extends ErlangElementRef {

	private final String record;

	public ErlangRecordRef(final String record) {
		super();
		this.record = record;
	}

	public String getRecord() {
		return record;
	}

	@Override
	public String toString() {
		return "#" + getRecord() + "{}";
	}

}

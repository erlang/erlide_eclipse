package org.erlide.core.search;

import org.erlide.core.erlang.IErlElement.Kind;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangRecordRef extends ErlangElementRef {

	private static final OtpErlangAtom RECORD_REF_ATOM = new OtpErlangAtom(
			"record_ref");
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
		return "#" + record + "{}";
	}

	@Override
	public OtpErlangObject getSearchObject() {
		return new OtpErlangTuple(new OtpErlangObject[] { RECORD_REF_ATOM,
				new OtpErlangAtom(record) });
	}

	@Override
	public String searchElementToString(final ErlangSearchElement ese) {
		return "#" + ese.getAttribute();
	}

	@Override
	public Kind searchElementToKind(final ErlangSearchElement ese) {
		return Kind.RECORD_DEF; // FIXME: ska vi ha en speciell RECORD_REF?
	}
}

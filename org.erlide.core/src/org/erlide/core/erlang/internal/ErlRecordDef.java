package org.erlide.core.erlang.internal;

import java.util.ArrayList;
import java.util.List;

import org.erlide.backend.util.Util;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlRecordDef;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlRecordDef extends ErlMember implements IErlRecordDef {

	private final String record;
	private final String extra;
	private final List<String> fields;

	/**
	 * @param parent
	 * @param imports
	 * @param module
	 */
	protected ErlRecordDef(final IErlElement parent, final String record,
			final String extra, final OtpErlangList fields) {
		super(parent, "record_definition");
		this.record = record;
		this.extra = extra;
		this.fields = new ArrayList<String>();
		if (fields != null) {
			for (final OtpErlangObject o : fields.elements()) {
				final OtpErlangAtom a = (OtpErlangAtom) o;
				this.fields.add(a.atomValue());
			}
		}
	}

	public ErlRecordDef(final IErlModule parent, final String recordName,
			final String s) {
		this(parent, recordName, s, null);
	}

	public Kind getKind() {
		return Kind.RECORD_DEF;
	}

	public String getDefinedName() {
		return record;
	}

	public List<String> getFields() {
		return fields;
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

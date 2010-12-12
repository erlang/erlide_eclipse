package org.erlide.core.erlang.internal;

import java.util.List;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlRecordDef;
import org.erlide.jinterface.backend.util.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class ErlRecordDef extends ErlMember implements IErlRecordDef {

    private final String record;
    private final String extra;
    private final List<String> fields;

    /**
     * @param parent
     * @param imports
     * @param module
     */
    protected ErlRecordDef(final IErlElement parent, final String extra,
            final OtpErlangList fields) {
        super(parent, "record_definition");
        record = uptoCommaOrParen(extra);
        this.extra = extra;
        this.fields = Lists.newArrayListWithCapacity(fields.arity());
        final List<ErlRecordField> children = Lists
                .newArrayListWithCapacity(fields.arity());
        if (fields != null) {
            for (final OtpErlangObject o : fields.elements()) {
                final OtpErlangTuple t = (OtpErlangTuple) o;
                final OtpErlangAtom fileNameA = (OtpErlangAtom) t.elementAt(0);
                final String fieldName = fileNameA.atomValue();
                final OtpErlangLong lineL = (OtpErlangLong) t.elementAt(1);
                final OtpErlangLong offsetL = (OtpErlangLong) t.elementAt(2);
                final OtpErlangLong lengthL = (OtpErlangLong) t.elementAt(3);
                this.fields.add(fieldName);
                try {
                    children.add(new ErlRecordField(this, fieldName, lineL
                            .intValue(), offsetL.intValue(), lengthL.intValue()));
                } catch (final OtpErlangRangeException e) {
                }
            }
            setChildren(children);
        }
    }

    public ErlRecordDef(final IErlModule parent, final String s) {
        this(parent, s, null);
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

package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlRecordDef;
import org.erlide.core.erlang.IErlRecordField;

public class ErlRecordField extends ErlMember implements IErlRecordField {

    private final String fieldName;

    protected ErlRecordField(final IErlRecordDef parent, final String name,
            final int line, final int offset, final int length) {
        super(parent, "record_field");
        fieldName = name;
        setLineStart(line);
        setLineEnd(line);
        setNameRange(offset, length);
        setSourceRangeOffset(offset);
        setSourceRangeLength(length);
    }

    public Kind getKind() {
        return Kind.RECORD_FIELD;
    }

    @Override
    public String toString() {
        return getName() + ": " + getFieldName();
    }

    public String getFieldName() {
        return fieldName;
    }

}

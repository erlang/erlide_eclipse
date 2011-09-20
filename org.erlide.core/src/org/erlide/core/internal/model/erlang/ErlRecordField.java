package org.erlide.core.internal.model.erlang;

import org.erlide.core.internal.model.root.ErlMember;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.core.model.erlang.IErlRecordField;

public class ErlRecordField extends ErlMember implements IErlRecordField {

    private final String fieldName;
    private String extra;

    public ErlRecordField(final IErlRecordDef parent, final String name) {
        super(parent, "record_field");
        fieldName = name;
        extra = "";
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

    public void setExtra(final String extra) {
        this.extra = extra;
    }

    public String getExtra() {
        return extra;
    }

}

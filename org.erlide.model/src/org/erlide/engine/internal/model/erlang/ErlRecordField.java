package org.erlide.engine.internal.model.erlang;

import org.erlide.engine.model.erlang.IErlRecordDef;
import org.erlide.engine.model.erlang.IErlRecordField;
import org.erlide.engine.model.root.ErlElementKind;

public class ErlRecordField extends ErlMember implements IErlRecordField {

    private final String fieldName;
    private String extra;

    public ErlRecordField(final IErlRecordDef parent, final String name) {
        super(parent, "record_field");
        fieldName = name;
        extra = "";
    }

    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.RECORD_FIELD;
    }

    @Override
    public String toString() {
        return getName() + ": " + getFieldName();
    }

    @Override
    public String getFieldName() {
        return fieldName;
    }

    public void setExtra(final String extra) {
        this.extra = extra;
    }

    @Override
    public String getExtra() {
        return extra;
    }

}

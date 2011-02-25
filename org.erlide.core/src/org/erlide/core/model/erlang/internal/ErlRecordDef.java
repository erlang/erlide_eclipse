package org.erlide.core.model.erlang.internal;

import org.erlide.core.common.Util;
import org.erlide.core.model.erlang.ErlModelException;
import org.erlide.core.model.erlang.IErlElement;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.core.model.erlang.IErlRecordField;
import org.erlide.core.model.erlang.IParent;

public class ErlRecordDef extends ErlMember implements IErlRecordDef {

    private final String record;
    private final String extra;

    /**
     * @param parent
     * @param imports
     * @param module
     */
    protected ErlRecordDef(final IParent parent, final String extra) {
        super(parent, "record_definition");
        record = uptoCommaOrParen(extra);
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

    public IErlRecordField getFieldNamed(final String name) {
        try {
            for (final IErlElement e : getChildren()) {
                if (e instanceof IErlRecordField) {
                    final IErlRecordField field = (IErlRecordField) e;
                    if (field.getFieldName().equals(name)) {
                        return field;
                    }
                }
            }
        } catch (final ErlModelException e) {
        }
        return null;
    }

}

package org.erlide.core.internal.model.erlang;

import org.erlide.core.internal.model.root.ErlElement;
import org.erlide.core.internal.model.root.ErlMember;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.core.model.erlang.IErlRecordField;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IParent;

import com.google.common.base.Objects;

public class ErlRecordDef extends ErlMember implements IErlRecordDef {

    private final String record;
    private final String extra;

    /**
     * @param parent
     * @param imports
     * @param module
     */
    public ErlRecordDef(final IParent parent, final String extra) {
        super(parent, "record_definition");
        record = uptoCommaOrParen(extra);
        this.extra = extra;
    }

    @Override
    public Kind getKind() {
        return Kind.RECORD_DEF;
    }

    @Override
    public String getDefinedName() {
        return record;
    }

    @Override
    public String toString() {
        return getName() + ": " + getDefinedName();
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(super.hashCode(), getDefinedName());
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null) {
            return false;
        }

        if (getParent() == null) {
            // this should never happen!
            return super.equals(o);
        }

        if (o instanceof ErlElement) {
            return toString().equals(o.toString());
        }
        return false;
    }

    @Override
    public String getExtra() {
        return extra;
    }

    @Override
    public IErlRecordField getFieldNamed(final String name) {
        try {
            for (final IErlElement e : getChildrenOfKind(Kind.RECORD_FIELD)) {
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

package org.erlide.engine.internal.model.erlang;

import org.erlide.engine.internal.model.root.ErlElement;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.IErlRecordDef;
import org.erlide.engine.model.erlang.IErlRecordField;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;

import com.google.common.base.Objects;

public class ErlRecordDef extends ErlMember implements IErlRecordDef {

    private final String record;
    private final String extra;

    /**
     * @param parent
     * @param imports
     * @param module
     */
    public ErlRecordDef(final IParent parent, final String name, final String extra) {
        super(parent, "record_definition");
        record = name != null ? name : uptoEndOfToken(extra);
        this.extra = extra;
    }

    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.RECORD_DEF;
    }

    @Override
    public String getDefinedName() {
        return record;
    }

    @Override
    public String toString() {
        return "record: " + getDefinedName();
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
            for (final IErlElement e : getChildrenOfKind(ErlElementKind.RECORD_FIELD)) {
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

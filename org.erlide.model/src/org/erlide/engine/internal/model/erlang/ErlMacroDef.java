package org.erlide.engine.internal.model.erlang;

import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.IErlMacroDef;
import org.erlide.engine.model.root.ErlElementKind;

import com.google.common.base.Objects;

public class ErlMacroDef extends ErlMember implements IErlMacroDef {

    String macro;
    String extra;

    public ErlMacroDef(final IParent parent, final String name, final String extra) {
        super(parent, "macro_definition");
        this.extra = extra;
        macro = name != null ? name : uptoEndOfToken(extra);
    }

    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.MACRO_DEF;
    }

    @Override
    public String getDefinedName() {
        return macro;
    }

    @Override
    public String toString() {
        return "macro: " + getDefinedName();
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(super.hashCode(), getDefinedName());
    }

    @Override
    public boolean equals(final Object o) {
        if (o instanceof ErlMacroDef) {
            final ErlMacroDef other = (ErlMacroDef) o;
            return super.equals(o) && macro.equals(other.macro);
        }
        return false;
    }

    @Override
    public String getExtra() {
        return extra;
    }

}

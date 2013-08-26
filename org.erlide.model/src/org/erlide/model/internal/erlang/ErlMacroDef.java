package org.erlide.model.internal.erlang;

import org.erlide.model.IParent;
import org.erlide.model.erlang.IErlMacroDef;
import org.erlide.model.internal.root.ErlMember;
import org.erlide.model.root.ErlElementKind;

import com.google.common.base.Objects;

public class ErlMacroDef extends ErlMember implements IErlMacroDef {

    String macro;
    String extra;

    /**
     * @param parent
     * @param imports
     * @param module
     */
    public ErlMacroDef(final IParent parent, final String macro,
            final String extra) {
        super(parent, "macro_definition");
        this.macro = macro;
        this.extra = extra;
    }

    public ErlMacroDef(final IParent parent, final String extra) {
        super(parent, "macro_definition");
        this.extra = extra;
        macro = uptoCommaOrParen(extra);
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
        return getName() + ": " + getDefinedName();
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

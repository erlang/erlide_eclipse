package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlMacroDef;
import org.erlide.core.erlang.IParent;
import org.erlide.jinterface.backend.util.Util;

public class ErlMacroDef extends ErlMember implements IErlMacroDef {

    String macro;
    String extra;

    /**
     * @param parent
     * @param imports
     * @param module
     */
    protected ErlMacroDef(final IParent parent, final String macro,
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

    public Kind getKind() {
        return Kind.MACRO_DEF;
    }

    public String getDefinedName() {
        return macro;
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

    /*
     * (non-Javadoc)
     * 
     * @see org.erlide.core.erlang.internal.ErlElement#equals(java.lang.Object)
     */
    @Override
    public boolean equals(final Object o) {
        if (o instanceof ErlMacroDef) {
            final ErlMacroDef other = (ErlMacroDef) o;
            return super.equals(o) && macro.equals(other.macro);
        }
        return false;
    }

    public String getExtra() {
        return extra;
    }

}

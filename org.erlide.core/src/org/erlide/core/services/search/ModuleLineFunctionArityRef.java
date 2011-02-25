package org.erlide.core.services.search;

public class ModuleLineFunctionArityRef {

    private final String modulePath;
    private final int offset, length;
    private final String name;
    private final int arity;
    private final String clauseHead;
    private final boolean subClause;
    private final boolean def;

    public ModuleLineFunctionArityRef(final String modulePath,
            final int offset, final int length, final String name,
            final int arity, final String clauseHead, final boolean subClause,
            final boolean def) {
        this.modulePath = modulePath;
        this.offset = offset;
        this.length = length;
        this.subClause = subClause;
        this.name = name;
        this.arity = arity;
        this.clauseHead = clauseHead;
        this.def = def;
    }

    public String getModulePath() {
        return modulePath;
    }

    public String getName() {
        return name;
    }

    public String getClauseHead() {
        return clauseHead;
    }

    public boolean isSubClause() {
        return subClause;
    }

    public int getLength() {
        return length;
    }

    public int getOffset() {
        return offset;
    }

    public int getArity() {
        return arity;
    }

    public boolean isDef() {
        return def;
    }
}

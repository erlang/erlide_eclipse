package org.erlide.ui.internal.search;

import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.IErlElement.Kind;

public class ErlangSearchElement {

    private final String moduleName;
    private final String name;
    private final int arity;
    private final String arguments;
    private final boolean subClause;
    private final Kind kind;
    private final IErlModule module;

    public ErlangSearchElement(final IErlModule module,
            final String moduleName, final String name, final int arity,
            final String arguments, final boolean subClause, final Kind kind) {
        this.module = module;
        this.moduleName = moduleName;
        this.name = name;
        this.arity = arity;
        this.arguments = arguments;
        this.subClause = subClause;
        this.kind = kind;
    }

    public String getModuleName() {
        return moduleName;
    }

    @Override
    public boolean equals(final Object o) {
        if (o instanceof ErlangSearchElement) {
            final ErlangSearchElement e = (ErlangSearchElement) o;
            if (e.moduleName.equals(moduleName) && e.name.equals(name)) {
                if (e.arity == arity) {
                    if (e.subClause == subClause) {
                        if (e.arguments == null) {
                            return arguments == null;
                        }
                        return e.arguments.equals(arguments);
                    }
                }
            }
        }
        return false;
    }

    @Override
    public int hashCode() {
        final int multiplier = 37; // some prime
        int hashCode = 13; // some random value
        if (subClause) {
            hashCode++;
        }
        hashCode = hashCode * multiplier + moduleName.hashCode();
        if (name != null) {
            hashCode = hashCode * multiplier + name.hashCode();
        }
        hashCode = hashCode * multiplier + arity;
        if (arguments != null) {
            hashCode = hashCode * multiplier + arguments.hashCode();
        }
        return hashCode;
    }

    public String getArguments() {
        return arguments;
    }

    public boolean isSubClause() {
        return subClause;
    }

    public Kind getKind() {
        return kind;
    }

    public String getName() {
        return name;
    }

    public int getArity() {
        return arity;
    }

    public IErlModule getModule() {
        return module;
    }

}

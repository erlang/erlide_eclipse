package org.erlide.core.services.search;

public abstract class NamePattern extends ErlangSearchPattern {

    private final String name;

    protected NamePattern(final String name, final LimitTo limitTo) {
        super(limitTo);
        this.name = name;
    }

    @Override
    public String patternString() {
        return getName();
    }

    public String getName() {
        return name;
    }

}

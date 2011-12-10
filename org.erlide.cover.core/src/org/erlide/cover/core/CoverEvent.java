package org.erlide.cover.core;

/**
 * Basic implementation of ICoverEvent
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class CoverEvent implements ICoverEvent {

    private final CoverStatus type;
    private final String info;

    public CoverEvent(final CoverStatus type, final String info) {
        this.type = type;
        this.info = info;
    }

    public CoverEvent(final CoverStatus type) {
        this(type, "");
    }

    @Override
    public CoverStatus getType() {
        return type;
    }

    @Override
    public String getInfo() {
        return info;
    }

}

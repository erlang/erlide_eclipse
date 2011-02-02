package org.erlide.cover.core;

/**
 * Basic implementation of ICoverEvent
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public class CoverEvent implements ICoverEvent {

    private CoverStatus type;
    private String info;
    
    public CoverEvent(CoverStatus type, String info) {
        this.type = type;
        this.info = info;
    }
    
    public CoverEvent(CoverStatus type) {
        this(type, "");
    }
    
    public CoverStatus getType() {
        return type;
    }

    public String getInfo() {
        return info;
    }

}

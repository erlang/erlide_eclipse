package org.erlide.cover.core;

/**
 * An interface for events occured while running coverage
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public interface ICoverEvent {
    
    public CoverStatus getType();
    
    public String getInfo();
    
}

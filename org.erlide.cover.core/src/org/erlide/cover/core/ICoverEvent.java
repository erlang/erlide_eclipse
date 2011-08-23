package org.erlide.cover.core;

/**
 * An interface for events occured while running coverage
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public interface ICoverEvent {

    /**
     * Get type of event
     * 
     * @return
     */
    public CoverStatus getType();

    /**
     * Get information message
     * 
     * @return
     */
    public String getInfo();

}

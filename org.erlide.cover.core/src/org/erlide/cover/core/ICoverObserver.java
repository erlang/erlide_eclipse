package org.erlide.cover.core;

/**
 * Listener interface for observing events in backend
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public interface ICoverObserver {

    public void eventOccured(ICoverEvent e);

}

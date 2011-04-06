package org.erlide.cover.api;

/**
 * Cover plugin exception class.
 * Thrown if something inside Cover plugin goes wrong
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public class CoverException extends Exception {

    public CoverException(String message) {
        super(message);
    }
    
}

package org.erlide.cover.core;

/**
 * Cover plugin exception clas.
 * Thrown if something inside Cover plugin goes wrong
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public class CoverException extends Exception {
    
    public CoverException() {
        super();
    }
    
    public CoverException(String mesg) {
        super(mesg);
    }

}

package org.erlide.cover.api;

/**
 * Cover plugin exception class. Thrown if something inside Cover plugin goes
 * wrong
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class CoverException extends Exception {

    private static final long serialVersionUID = 1L;

    public CoverException(final String message) {
        super(message);
    }

    public CoverException(final Exception e) {
        super(e);
    }

}

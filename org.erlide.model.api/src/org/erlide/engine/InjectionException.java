package org.erlide.engine;

public class InjectionException extends RuntimeException {

    public InjectionException(final String message) {
        super(message);
    }

    public InjectionException(final String string, final Exception e) {
        super(string, e);
    }

    private static final long serialVersionUID = 1L;

}

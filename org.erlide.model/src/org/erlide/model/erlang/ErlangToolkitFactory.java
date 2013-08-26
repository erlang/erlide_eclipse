package org.erlide.model.erlang;

public class ErlangToolkitFactory {

    private static final ErlangToolkit instance = new ErlangBackendToolkit();

    public static ErlangToolkit getInstance() {
        return instance;
    }

}

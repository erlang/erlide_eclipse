package org.erlide.model.erlang;


public class ErlangToolkitFactory {

    private static volatile ErlangToolkit instance;

    public static ErlangToolkit getInstance() {
        if (instance == null) {
            instance = new ErlangBackendToolkit();
        }
        return instance;
    }

}

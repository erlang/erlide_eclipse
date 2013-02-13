package org.erlide.model.erlang;

import org.erlide.model.erlang.ErlangToolkit;

public class ErlangToolkitFactory {

    private static volatile ErlangToolkit instance;

    public static ErlangToolkit getInstance() {
        if (instance == null) {
            instance = new ErlangBackendToolkit();
        }
        return instance;
    }

}

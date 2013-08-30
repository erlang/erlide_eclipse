package org.erlide.model;

import org.erlide.engine.ErlangEngine;
import org.erlide.model.root.IErlServiceProvider;
import org.erlide.util.ErlLogger;

public class ErlServiceProvider implements IErlServiceProvider {

    private final ErlangEngine engine;

    public ErlServiceProvider() {
        engine = ErlangEngine.getInstance();
    }

    @Override
    public <T> T get(final Class<T> serviceClass) {
        try {
            return serviceClass.cast(engine);
        } catch (final ClassCastException e) {
            ErlLogger.error(e);
            // TODO should return a dummy, maybe
            return null;
        }
    }

}

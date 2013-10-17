package org.erlide.core.internal.builder;

import org.erlide.util.ErlLogger;

public enum BuildersInfo {
    INTERNAL(InternalBuilder.class), MAKE(MakeBuilder.class), EMAKE(EmakeBuilder.class), REBAR(
            RebarBuilder.class);

    final private Class<? extends ErlangBuilder> impl;

    BuildersInfo(final Class<? extends ErlangBuilder> impl) {
        this.impl = impl;
    }

    public ErlangBuilder getImpl() {
        try {
            return impl.newInstance();
        } catch (final InstantiationException e) {
            ErlLogger.error(e);
        } catch (final IllegalAccessException e) {
            ErlLogger.error(e);
        }
        return null;
    }

}

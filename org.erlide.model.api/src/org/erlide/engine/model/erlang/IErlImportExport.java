package org.erlide.engine.model.erlang;

import java.util.Collection;

public interface IErlImportExport {
    public boolean hasFunction(final ErlangFunction f);

    public Collection<ErlangFunction> getFunctions();
}

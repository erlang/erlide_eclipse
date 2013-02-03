package org.erlide.core.model.erlang;

import java.util.Collection;

import org.erlide.core.model.util.ErlangFunction;

public interface IErlImportExport {
    public boolean hasFunction(final ErlangFunction f);

    public Collection<ErlangFunction> getFunctions();
}

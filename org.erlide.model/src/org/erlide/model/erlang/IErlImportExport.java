package org.erlide.model.erlang;

import java.util.Collection;

import org.erlide.model.util.ErlangFunction;

public interface IErlImportExport {
    public boolean hasFunction(final ErlangFunction f);

    public Collection<ErlangFunction> getFunctions();
}

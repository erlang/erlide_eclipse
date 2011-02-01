package org.erlide.core.erlang;

import java.util.Collection;

import org.erlide.core.erlang.util.ErlangFunction;

public interface IErlImportExport {
    public boolean hasFunction(final ErlangFunction f);

    public Collection<ErlangFunction> getFunctions();
}

package org.erlide.core.erlang;

import java.util.List;

import org.erlide.core.erlang.util.ErlangFunction;

public interface IErlImportExport {
    public boolean hasFunction(final ErlangFunction f);

    public List<ErlangFunction> getFunctions();
}

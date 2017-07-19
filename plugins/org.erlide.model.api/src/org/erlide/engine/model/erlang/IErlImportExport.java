package org.erlide.engine.model.erlang;

import java.util.Collection;

/**
 *
 * @author Vlad
 *
 * @noimplement This interface is not intended to be implemented by clients.
 *
 */
public interface IErlImportExport {
    public boolean hasFunction(final ErlangFunction f);

    public Collection<ErlangFunction> getFunctions();
}

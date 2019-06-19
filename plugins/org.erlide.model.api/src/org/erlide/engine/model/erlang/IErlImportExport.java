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
    boolean hasFunction(final ErlangFunction f);

    Collection<ErlangFunction> getFunctions();
}

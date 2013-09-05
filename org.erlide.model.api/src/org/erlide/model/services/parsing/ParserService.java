package org.erlide.model.services.parsing;

import org.erlide.model.erlang.IErlModule;

public interface ParserService {

    // TODO this API is not good at all, better split it in multiple methods
    // according to which parameters are used

    public abstract boolean parse(final IErlModule module,
            final String scannerName, final boolean initialParse,
            final String path, String initialText, boolean updateSearchServer);

}

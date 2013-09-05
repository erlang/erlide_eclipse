package org.erlide.model.services.scanner;

import org.erlide.model.erlang.IErlModule;

public interface ParserService {

    public abstract boolean parse(final IErlModule module,
            final String scannerName, final boolean initialParse,
            final String path, String initialText, boolean updateSearchServer);

}

package org.erlide.core.model.root;

import org.erlide.core.model.erlang.IErlModule;

public interface IErlParser {

    public abstract boolean parse(final IErlModule module,
            final String scannerName, final boolean initialParse,
            final String path, final boolean useCaches);

}

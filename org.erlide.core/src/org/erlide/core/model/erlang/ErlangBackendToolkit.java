package org.erlide.core.model.erlang;

import org.erlide.core.internal.model.erlang.ErlParser;
import org.erlide.core.internal.model.erlang.ErlScanner;

public class ErlangBackendToolkit implements ErlangToolkit {

    @Override
    public IErlParser createParser() {
        return new ErlParser();
    }

    @Override
    public IErlScanner createScanner(final String scannerName,
            final String initialText, final String path, final boolean useCaches) {
        return new ErlScanner(scannerName, initialText, path, useCaches);
    }

}

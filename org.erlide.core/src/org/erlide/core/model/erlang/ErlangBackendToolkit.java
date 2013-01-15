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
            final String initialText, final String path, final boolean logging) {
        final ErlScanner scanner = new ErlScanner(scannerName);
        scanner.initialScan(initialText, path, logging);
        return scanner;
    }

}

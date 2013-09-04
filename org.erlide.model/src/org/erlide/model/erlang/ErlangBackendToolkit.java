package org.erlide.model.erlang;

import org.erlide.model.internal.erlang.ErlParser;
import org.erlide.model.internal.erlang.ErlScanner;
import org.erlide.runtime.api.IRpcSite;

public class ErlangBackendToolkit implements ErlangToolkit {

    private final IRpcSite backend;

    public ErlangBackendToolkit(final IRpcSite backend) {
        this.backend = backend;
    }

    @Override
    public IErlParser createParser() {
        return new ErlParser(backend);
    }

    @Override
    public IErlScanner createScanner(final String scannerName,
            final String initialText, final String path, final boolean logging) {
        final ErlScanner scanner = new ErlScanner(scannerName);
        scanner.initialScan(initialText, path, logging);
        return scanner;
    }

}

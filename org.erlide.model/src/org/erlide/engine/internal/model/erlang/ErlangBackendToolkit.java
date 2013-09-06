package org.erlide.engine.internal.model.erlang;

import org.erlide.engine.model.erlang.ErlangToolkit;
import org.erlide.engine.services.parsing.ParserService;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.runtime.api.IRpcSite;

public class ErlangBackendToolkit implements ErlangToolkit {

    private final IRpcSite backend;

    public ErlangBackendToolkit(final IRpcSite backend) {
        this.backend = backend;
    }

    @Override
    public ParserService getParserService() {
        return new ErlParser(backend);
    }

    @Override
    public ScannerService getScannerService(final String scannerName,
            final String initialText, final String path, final boolean logging) {
        final ErlScanner scanner = new ErlScanner(backend, scannerName);
        scanner.initialScan(initialText, path, logging);
        return scanner;
    }

}

package org.erlide.model.erlang;

import org.erlide.model.internal.erlang.ErlParser;
import org.erlide.model.internal.erlang.ErlScanner;
import org.erlide.model.services.scanner.ParserService;
import org.erlide.model.services.scanner.ScannerService;
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
        final ErlScanner scanner = new ErlScanner(scannerName);
        scanner.initialScan(initialText, path, logging);
        return scanner;
    }

}

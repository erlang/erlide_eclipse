package org.erlide.engine.internal.services.parsing;

import org.erlide.engine.internal.model.erlang.ErlScanner;
import org.erlide.engine.services.parsing.ScannerProviderService;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.runtime.api.IRpcSite;

public class ScannerProvider implements ScannerProviderService {

    private final IRpcSite backend;

    public ScannerProvider(final IRpcSite backend) {
        this.backend = backend;
    }

    @Override
    public ScannerService get(final String scannerName,
            final String initialText, final String path, final boolean logging) {
        final ErlScanner scanner = new ErlScanner(backend, scannerName);
        scanner.initialScan(initialText, path, logging);
        return scanner;
    }

}

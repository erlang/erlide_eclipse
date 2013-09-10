package org.erlide.engine.internal.services.parsing;

import org.erlide.engine.services.parsing.ScannerProviderService;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.runtime.api.IRpcSite;

public class ScannerProvider implements ScannerProviderService {

    private final IRpcSite backend;

    public ScannerProvider(final IRpcSite backend) {
        this.backend = backend;
    }

    @Override
    public ScannerService get(final String scannerName) {
        return new ErlScanner(backend, scannerName);
    }

}

package org.erlide.engine.internal.services.parsing;

import org.erlide.engine.services.parsing.ScannerProviderService;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.runtime.api.IOtpRpc;

public class ScannerProvider implements ScannerProviderService {

    private final IOtpRpc backend;

    public ScannerProvider(final IOtpRpc backend) {
        this.backend = backend;
    }

    @Override
    public ScannerService get(final String scannerName) {
        return new ErlScanner(backend, scannerName);
    }

}

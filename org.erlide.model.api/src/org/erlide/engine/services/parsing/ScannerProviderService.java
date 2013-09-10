package org.erlide.engine.services.parsing;

import org.erlide.engine.services.ErlangService;

public interface ScannerProviderService extends ErlangService {

    ScannerService get(final String scannerName, final String initialText,
            final String path, final boolean logging);

}

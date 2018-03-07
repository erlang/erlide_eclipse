package org.erlide.engine.services.parsing;

import org.erlide.engine.services.ErlangService;

public interface ScannerProviderService extends ErlangService {

    ScannerService get(String scannerName);

}

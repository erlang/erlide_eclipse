package org.erlide.engine.services.parsing;

import java.util.List;

public interface SimpleScannerService {

    List<ErlToken> lightScanString(String string, int offset) throws ScannerException;

}

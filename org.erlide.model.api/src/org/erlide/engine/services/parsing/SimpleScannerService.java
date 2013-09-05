package org.erlide.engine.services.parsing;

import java.util.List;

import org.erlide.engine.model.erlang.ErlToken;

public interface SimpleScannerService {

    List<ErlToken> lightScanString(String string, int offset)
            throws ScannerException;

}

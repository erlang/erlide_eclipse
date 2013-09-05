package org.erlide.model.services.parsing;

import java.util.List;

import org.erlide.model.erlang.ErlToken;

public interface SimpleScannerService {

    List<ErlToken> lightScanString(String string, int offset)
            throws ScannerException;

}

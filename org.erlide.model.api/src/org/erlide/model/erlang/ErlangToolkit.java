package org.erlide.model.erlang;

import org.erlide.model.services.scanner.ParserService;
import org.erlide.model.services.scanner.ScannerService;

public interface ErlangToolkit {

    ParserService getParserService();

    ScannerService getScannerService(String scannerName, String initialText,
            String path, boolean logging);

}

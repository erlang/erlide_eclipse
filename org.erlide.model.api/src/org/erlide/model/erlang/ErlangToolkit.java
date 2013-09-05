package org.erlide.model.erlang;

import org.erlide.model.services.parsing.ParserService;
import org.erlide.model.services.parsing.ScannerService;

public interface ErlangToolkit {

    ParserService getParserService();

    ScannerService getScannerService(String scannerName, String initialText,
            String path, boolean logging);

}

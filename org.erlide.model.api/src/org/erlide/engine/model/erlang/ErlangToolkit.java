package org.erlide.engine.model.erlang;

import org.erlide.engine.services.parsing.ParserService;
import org.erlide.engine.services.parsing.ScannerService;

public interface ErlangToolkit {

    ParserService getParserService();

    ScannerService getScannerService(String scannerName, String initialText,
            String path, boolean logging);

}

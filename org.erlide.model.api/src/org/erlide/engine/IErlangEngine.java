package org.erlide.engine;

import org.erlide.model.root.IErlModel;
import org.erlide.model.services.codeassist.ContextAssistService;
import org.erlide.model.services.scanner.ScannerService;
import org.erlide.model.services.search.OpenService;
import org.erlide.model.services.search.OtpDocService;
import org.erlide.model.services.search.SearchServerService;
import org.erlide.model.services.search.XrefService;
import org.erlide.model.services.text.IndentService;

public interface IErlangEngine {

    IErlModel getModel();

    XrefService getXrefService();

    String getStateDir();

    OpenService getOpenService();

    OtpDocService getOtpDocService();

    IndentService getIndentService();

    ContextAssistService getContextAssistService();

    ScannerService getScannerService();

    SearchServerService getSearchServerService();

}

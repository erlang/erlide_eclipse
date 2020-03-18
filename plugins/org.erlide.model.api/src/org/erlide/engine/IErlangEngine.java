package org.erlide.engine;

import org.erlide.engine.model.root.IErlModel;
import org.erlide.engine.services.SystemInfoService;
import org.erlide.engine.services.ToggleCommentService;
import org.erlide.engine.services.cleanup.CleanupProvider;
import org.erlide.engine.services.codeassist.CompletionService;
import org.erlide.engine.services.edoc.EdocExportService;
import org.erlide.engine.services.parsing.ScannerProviderService;
import org.erlide.engine.services.parsing.SimpleParserService;
import org.erlide.engine.services.parsing.SimpleScannerService;
import org.erlide.engine.services.proclist.ProclistService;
import org.erlide.engine.services.search.ModelFindService;
import org.erlide.engine.services.search.ModelUtilService;
import org.erlide.engine.services.search.OpenService;
import org.erlide.engine.services.search.OtpDocService;
import org.erlide.engine.services.search.SearchServerService;
import org.erlide.engine.services.text.IndentService;

@SuppressWarnings("all")
public interface IErlangEngine {
    void initialize(final ErlangInitializeParams params);

    void shutdown();

    boolean isAvailable();

    IErlModel getModel();

    ModelUtilService getModelUtilService();

    CompletionService getCompletionService();

    CleanupProvider getCleanupProvider();

    SimpleScannerService getSimpleScannerService();

    SearchServerService getSearchServerService();

    ModelFindService getModelFindService();

    ToggleCommentService getToggleCommentService();

    IndentService getIndentService();

    OpenService getOpenService();

    OtpDocService getOtpDocService();

    EdocExportService getEdocExportService();

    SystemInfoService getSystemInfoService();

    ProclistService getProclistService();

    String getStateDir();

    ScannerProviderService getScannerProviderService();

    SimpleParserService getSimpleParserService();
}

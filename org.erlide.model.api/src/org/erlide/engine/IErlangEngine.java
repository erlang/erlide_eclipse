package org.erlide.engine;

import org.eclipse.core.resources.IResource;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.services.ErlangService;
import org.erlide.engine.services.cleanup.CleanupProvider;
import org.erlide.engine.services.codeassist.ContextAssistService;
import org.erlide.engine.services.edoc.EdocExportService;
import org.erlide.engine.services.importer.ImportService;
import org.erlide.engine.services.parsing.ParserService;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.engine.services.parsing.SimpleScannerService;
import org.erlide.engine.services.proclist.ProclistService;
import org.erlide.engine.services.search.ModelFindService;
import org.erlide.engine.services.search.ModelSearcherService;
import org.erlide.engine.services.search.ModelUtilService;
import org.erlide.engine.services.search.OpenService;
import org.erlide.engine.services.search.SearchServerService;
import org.erlide.runtime.api.IRpcSite;

public interface IErlangEngine {

    <T extends ErlangService> T getService(Class<T> type);

    @Deprecated
    IRpcSite getBackend();

    IErlModel getModel();

    String getStateDir();

    // //

    OpenService getOpenService();

    ContextAssistService getContextAssistService();

    SearchServerService getSearchServerService();

    ModelUtilService getModelUtilService();

    CleanupProvider getCleanupProvider(final IResource resource);

    ParserService getParserService();

    ScannerService getScannerService(String scannerName, String initialText,
            String path, boolean logging);

    ImportService getImportService();

    EdocExportService getEdocExportService();

    ProclistService getProclistService();

    ScannerService getScannerService(String name);

    SimpleScannerService getSimpleScannerService();

    ModelFindService getModelFindService();

    ModelSearcherService getModelSearcherService();

}

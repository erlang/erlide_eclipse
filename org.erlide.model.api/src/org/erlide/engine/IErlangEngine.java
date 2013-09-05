package org.erlide.engine;

import org.eclipse.core.resources.IResource;
import org.erlide.engine.model.erlang.ErlangToolkit;
import org.erlide.engine.model.root.IErlModel;
import org.erlide.engine.services.cleanup.CleanupProvider;
import org.erlide.engine.services.codeassist.ContextAssistService;
import org.erlide.engine.services.edoc.EdocExportService;
import org.erlide.engine.services.importer.ImportService;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.engine.services.parsing.SimpleScannerService;
import org.erlide.engine.services.proclist.ProclistService;
import org.erlide.engine.services.search.OpenService;
import org.erlide.engine.services.search.OtpDocService;
import org.erlide.engine.services.search.SearchServerService;
import org.erlide.engine.services.search.XrefService;
import org.erlide.engine.services.text.IndentService;
import org.erlide.engine.util.ModelUtilService;
import org.erlide.runtime.api.IRpcSite;

public interface IErlangEngine {

    @Deprecated
    IRpcSite getBackend();

    IErlModel getModel();

    XrefService getXrefService();

    String getStateDir();

    OpenService getOpenService();

    OtpDocService getOtpDocService();

    IndentService getIndentService();

    ContextAssistService getContextAssistService();

    SearchServerService getSearchServerService();

    ModelUtilService getModelUtilService();

    CleanupProvider getCleanupProvider(final IResource resource);

    ErlangToolkit getToolkit();

    ImportService getImportService();

    EdocExportService getEdocExportService();

    ProclistService getProclistService();

    ScannerService getScannerService(String name);

    SimpleScannerService getSimpleScannerService();

}

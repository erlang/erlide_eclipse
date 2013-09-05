package org.erlide.engine;

import org.eclipse.core.resources.IResource;
import org.erlide.model.erlang.ErlangToolkit;
import org.erlide.model.root.IErlModel;
import org.erlide.model.services.cleanup.CleanupProvider;
import org.erlide.model.services.codeassist.ContextAssistService;
import org.erlide.model.services.edoc.EdocExportService;
import org.erlide.model.services.importer.ImportService;
import org.erlide.model.services.proclist.ProclistService;
import org.erlide.model.services.scanner.BasicScannerService;
import org.erlide.model.services.search.OpenService;
import org.erlide.model.services.search.OtpDocService;
import org.erlide.model.services.search.SearchServerService;
import org.erlide.model.services.search.XrefService;
import org.erlide.model.services.text.IndentService;
import org.erlide.model.util.ModelUtilService;
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

    BasicScannerService getScannerService();

    SearchServerService getSearchServerService();

    ModelUtilService getModelUtilService();

    CleanupProvider getCleanupProvider(final IResource resource);

    ErlangToolkit getToolkit();

    ImportService getImportService();

    EdocExportService getEdocExportService();

    ProclistService getProclistService();

}

package org.erlide.engine.internal;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Platform;
import org.erlide.engine.ErlModelException;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.internal.model.erlang.ErlScanner;
import org.erlide.engine.internal.model.erlang.ErlangBackendToolkit;
import org.erlide.engine.internal.model.erlang.ModelFindUtil;
import org.erlide.engine.internal.model.erlang.ModelInternalUtils;
import org.erlide.engine.internal.model.root.ErlModel;
import org.erlide.engine.internal.services.cleanup.ErlTidyCleanupProvider;
import org.erlide.engine.internal.services.codeassist.ErlideContextAssist;
import org.erlide.engine.internal.services.edoc.ErlideEdocExport;
import org.erlide.engine.internal.services.importer.ErlideImport;
import org.erlide.engine.internal.services.parsing.ErlideScanner;
import org.erlide.engine.internal.services.proclist.ErlideProclist;
import org.erlide.engine.internal.services.search.ErlangXref;
import org.erlide.engine.internal.services.search.ErlideDoc;
import org.erlide.engine.internal.services.search.ErlideOpen;
import org.erlide.engine.internal.services.search.ErlideSearchServer;
import org.erlide.engine.internal.services.text.ErlideIndent;
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
import org.erlide.engine.util.ModelFindService;
import org.erlide.engine.util.ModelUtilService;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IRpcSiteProvider;
import org.erlide.util.ErlLogger;
import org.erlide.util.services.ExtensionUtils;
import org.osgi.framework.Bundle;

public class DefaultErlangEngine implements IErlangEngine {

    private final IRpcSite backend;

    public DefaultErlangEngine() {
        // TODO how to inject runtime and other start params?

        final IRpcSiteProvider provider = ExtensionUtils.getSingletonExtension(
                "org.erlide.backend.backend", IRpcSiteProvider.class);
        backend = provider.get();
    }

    @Deprecated
    @Override
    public IRpcSite getBackend() {
        return backend;
    }

    private volatile static IErlModel erlangModel;

    @Override
    public IErlModel getModel() {
        if (erlangModel == null) {
            final ErlangToolkit toolkit = ErlangEngine.getInstance()
                    .getToolkit();
            erlangModel = new ErlModel(toolkit);
        }
        if (!erlangModel.isOpen()) {
            try {
                erlangModel.open(null);
            } catch (final ErlModelException e) {
                ErlLogger.error(e);
            }
        }
        return erlangModel;
    }

    @Override
    public XrefService getXrefService() {
        return new ErlangXref(backend);
    }

    private volatile String stateDirCached;

    @Override
    public String getStateDir() {
        if (stateDirCached == null) {
            final Bundle modelPlugin = Platform
                    .getBundle(ModelPlugin.PLUGIN_ID);
            stateDirCached = Platform.getStateLocation(modelPlugin)
                    .toPortableString();
        }
        return stateDirCached;
    }

    @Override
    public OpenService getOpenService() {
        return new ErlideOpen(backend, getStateDir());
    }

    @Override
    public OtpDocService getOtpDocService() {
        return new ErlideDoc(backend);
    }

    @Override
    public IndentService getIndentService() {
        return new ErlideIndent(backend);
    }

    @Override
    public ContextAssistService getContextAssistService() {
        return new ErlideContextAssist(backend);
    }

    @Override
    public SearchServerService getSearchServerService() {
        return new ErlideSearchServer(backend);
    }

    @Override
    public ModelUtilService getModelUtilService() {
        return new ModelInternalUtils(backend);
    }

    @Override
    public ModelFindService getModelFindService() {
        return new ModelFindUtil(backend);
    }

    /**
     * <p>
     * Construct a {@link CleanUpProvider} appropriate for a particular
     * {@link IResource}.
     * </p>
     * 
     * @param resource
     *            {@link IResource} for the Erlang module to clean up
     * 
     * @return {@link CleanUpProvider} appropriate for the supplied
     *         {@link IResource}
     */
    @Override
    public CleanupProvider getCleanupProvider(final IResource resource) {
        return new ErlTidyCleanupProvider(backend, resource);
    }

    @Override
    public ErlangToolkit getToolkit() {
        return new ErlangBackendToolkit(backend);
    }

    @Override
    public ImportService getImportService() {
        return new ErlideImport(backend);
    }

    @Override
    public EdocExportService getEdocExportService() {
        return new ErlideEdocExport(backend);
    }

    @Override
    public ProclistService getProclistService() {
        return new ErlideProclist();
    }

    @Override
    public ScannerService getScannerService(final String name) {
        return new ErlScanner(backend, name);
    }

    @Override
    public SimpleScannerService getSimpleScannerService() {
        return new ErlideScanner(backend);
    }
}

package org.erlide.engine.internal;

import java.util.Map;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Platform;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.internal.model.BeamLocator;
import org.erlide.engine.internal.model.ErlModel;
import org.erlide.engine.internal.model.erlang.ErlParser;
import org.erlide.engine.internal.model.erlang.ErlScanner;
import org.erlide.engine.internal.model.erlang.ModelFindUtil;
import org.erlide.engine.internal.model.erlang.ModelInternalUtils;
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
import org.erlide.engine.internal.services.search.ModelSearcher;
import org.erlide.engine.internal.services.text.ErlideIndent;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IBeamLocator;
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
import org.erlide.engine.services.search.OtpDocService;
import org.erlide.engine.services.search.SearchServerService;
import org.erlide.engine.services.search.XrefService;
import org.erlide.engine.services.text.IndentService;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IRpcSiteProvider;
import org.erlide.util.ErlLogger;
import org.erlide.util.services.ExtensionUtils;
import org.osgi.framework.Bundle;

import com.google.common.collect.Maps;

public class DefaultErlangEngine implements IErlangEngine {

    private final Map<Class<? extends ErlangService>, Class<? extends ErlangService>> implementations = Maps
            .newHashMap();

    @Override
    public <T extends ErlangService> T get(final Class<T> type) {
        try {
            return (T) implementations.get(type).newInstance();
        } catch (final InstantiationException e) {
            e.printStackTrace();
        } catch (final IllegalAccessException e) {
            e.printStackTrace();
        }
        return null;
    }

    private final IRpcSite backend;

    public DefaultErlangEngine() {
        // TODO how to inject runtime and other start params?

        final IRpcSiteProvider provider = ExtensionUtils.getSingletonExtension(
                "org.erlide.backend.backend", IRpcSiteProvider.class);
        backend = provider.get();

        implementations.put(XrefService.class, ErlangXref.class);

    }

    @Deprecated
    @Override
    public IRpcSite getBackend() {
        return backend;
    }

    private volatile static ErlModel erlangModel;

    @Override
    public IErlModel getModel() {
        if (erlangModel == null) {
            erlangModel = new ErlModel();
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
        return get(XrefService.class);
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
    public ParserService getParserService() {
        return new ErlParser(backend);
    }

    @Override
    public ScannerService getScannerService(final String scannerName,
            final String initialText, final String path, final boolean logging) {
        final ErlScanner scanner = new ErlScanner(backend, scannerName);
        scanner.initialScan(initialText, path, logging);
        return scanner;
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

    @Override
    public ModelSearcherService getModelSearcherService() {
        return new ModelSearcher();
    }

    @Override
    public IBeamLocator getBeamLocator() {
        return new BeamLocator();
    }

}

package org.erlide.engine.internal;

import java.lang.reflect.Constructor;
import java.util.Map;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Platform;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.InjectionException;
import org.erlide.engine.internal.model.BeamLocator;
import org.erlide.engine.internal.model.ErlModel;
import org.erlide.engine.internal.model.erlang.ModelFindUtil;
import org.erlide.engine.internal.model.erlang.ModelInternalUtils;
import org.erlide.engine.internal.services.cleanup.ErlTidyCleanupProvider;
import org.erlide.engine.internal.services.codeassist.ErlideContextAssist;
import org.erlide.engine.internal.services.edoc.ErlideEdocExport;
import org.erlide.engine.internal.services.importer.ErlideImport;
import org.erlide.engine.internal.services.parsing.ErlParser;
import org.erlide.engine.internal.services.parsing.ErlideScanner;
import org.erlide.engine.internal.services.parsing.ScannerProvider;
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
import org.erlide.engine.services.parsing.ScannerProviderService;
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
    public <T extends ErlangService> T getService(final Class<T> type) {
        try {
            final Class<? extends ErlangService> clazz = implementations
                    .get(type);
            if (clazz == null) {
                throw new InjectionException(
                        "ErlangService implementation not found for "
                                + type.getName());
            }

            final Constructor<?> constructor = clazz.getConstructors()[0];
            final Class<?>[] parameterTypes = constructor.getParameterTypes();
            final Object[] initargs = new Object[parameterTypes.length];
            for (int i = 0; i < parameterTypes.length; i++) {
                final Class<?> paramType = parameterTypes[i];
                initargs[i] = injectParameter(paramType);
            }
            return (T) constructor.newInstance(initargs);
        } catch (final Exception e) {
            throw new InjectionException("Could not instantiate service "
                    + type.getName(), e);
        }
    }

    private Object injectParameter(final Class<?> paramType) {
        if (IRpcSite.class == paramType) {
            return backend;
        }
        if (IErlModel.class == paramType) {
            return erlangModel;
        }
        if (String.class == paramType) {
            return stateDir;
        }
        throw new InjectionException(
                "Constructor parameters are not injectable (ErlangService): "
                        + paramType.getName());
    }

    public DefaultErlangEngine() {
        // TODO how to inject runtime and other start params?

        final IRpcSiteProvider provider = ExtensionUtils.getSingletonExtension(
                "org.erlide.backend.backend", IRpcSiteProvider.class);
        backend = provider.get();

        // TODO use exension points?

        implementations.put(XrefService.class, ErlangXref.class);
        implementations.put(IndentService.class, ErlideIndent.class);
        implementations.put(OtpDocService.class, ErlideDoc.class);
        implementations.put(IBeamLocator.class, BeamLocator.class);
    }

    private final IRpcSite backend;

    @Deprecated
    @Override
    public IRpcSite getBackend() {
        return backend;
    }

    private volatile ErlModel erlangModel;

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

    private volatile String stateDir;

    @Override
    public String getStateDir() {
        if (stateDir == null) {
            final Bundle modelPlugin = Platform
                    .getBundle(ModelPlugin.PLUGIN_ID);
            stateDir = Platform.getStateLocation(modelPlugin)
                    .toPortableString();
        }
        return stateDir;
    }

    @Override
    public OpenService getOpenService() {
        return new ErlideOpen(backend, getStateDir());
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
    public CleanupProvider getCleanupProvider() {
        return new ErlTidyCleanupProvider(backend);
    }

    @Override
    public ParserService getParserService() {
        return new ErlParser(backend);
    }

    @Override
    public ScannerProviderService getScannerProviderService() {
        return new ScannerProvider(backend);
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
    public SimpleScannerService getSimpleScannerService() {
        return new ErlideScanner(backend);
    }

    @Override
    public ModelSearcherService getModelSearcherService() {
        return new ModelSearcher();
    }

}

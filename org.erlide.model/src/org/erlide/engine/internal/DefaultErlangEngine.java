package org.erlide.engine.internal;

import java.lang.reflect.Constructor;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.Platform;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.InjectionException;
import org.erlide.engine.internal.model.BeamLocator;
import org.erlide.engine.internal.model.ErlModel;
import org.erlide.engine.internal.model.erlang.ModelFindUtil;
import org.erlide.engine.internal.model.erlang.ModelInternalUtils;
import org.erlide.engine.internal.model.root.ProjectConfiguratorFactory;
import org.erlide.engine.internal.services.cleanup.ErlTidyCleanupProvider;
import org.erlide.engine.internal.services.codeassist.ErlideContextAssist;
import org.erlide.engine.internal.services.edoc.ErlideEdocExport;
import org.erlide.engine.internal.services.importer.ErlideImport;
import org.erlide.engine.internal.services.parsing.ErlParser;
import org.erlide.engine.internal.services.parsing.ErlideParser;
import org.erlide.engine.internal.services.parsing.ErlideScanner;
import org.erlide.engine.internal.services.parsing.ScannerProvider;
import org.erlide.engine.internal.services.proclist.ErlideProclist;
import org.erlide.engine.internal.services.search.ErlangXref;
import org.erlide.engine.internal.services.search.ErlideDoc;
import org.erlide.engine.internal.services.search.ErlideOpen;
import org.erlide.engine.internal.services.search.ErlideSearchServer;
import org.erlide.engine.internal.services.search.ModelSearcher;
import org.erlide.engine.internal.services.text.ErlideIndent;
import org.erlide.engine.model.IBeamLocator;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.root.IProjectConfiguratorFactory;
import org.erlide.engine.services.ErlangService;
import org.erlide.engine.services.GenericService;
import org.erlide.engine.services.SystemInfoService;
import org.erlide.engine.services.cleanup.CleanupProvider;
import org.erlide.engine.services.codeassist.ContextAssistService;
import org.erlide.engine.services.edoc.EdocExportService;
import org.erlide.engine.services.importer.ImportService;
import org.erlide.engine.services.parsing.NullScannerService;
import org.erlide.engine.services.parsing.ParserService;
import org.erlide.engine.services.parsing.ScannerProviderService;
import org.erlide.engine.services.parsing.SimpleParserService;
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
import org.erlide.engine.util.OtpRpcFactory;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.collect.Maps;

public class DefaultErlangEngine implements IErlangEngine, IExecutableExtension {

    private final Map<Class<? extends ErlangService>, Class<? extends ErlangService>> implementations = Maps
            .newHashMap();

    @SuppressWarnings("unchecked")
    @Override
    public <T extends ErlangService> T getService(final Class<T> type) {
        try {
            final Class<? extends ErlangService> clazz = implementations.get(type);
            if (clazz == null) {
                throw new InjectionException(
                        "ErlangService implementation not found for " + type.getName());
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
        if (IOtpRpc.class == paramType) {
            return backend;
        }
        if (IErlModel.class == paramType) {
            return getModel();
        }
        if (String.class == paramType) {
            return getStateDir();
        }
        throw new InjectionException(
                "Constructor parameters are not injectable (ErlangService): "
                        + paramType.getName());
    }

    public DefaultErlangEngine() {
        // TODO how to inject runtime and other start params?

        // TODO use extension points?

        implementations.put(XrefService.class, ErlangXref.class);
        implementations.put(IndentService.class, ErlideIndent.class);
        implementations.put(OtpDocService.class, ErlideDoc.class);
        implementations.put(IBeamLocator.class, BeamLocator.class);
        implementations.put(OpenService.class, ErlideOpen.class);
        implementations.put(SystemInfoService.class, SystemInfo.class);
    }

    private IOtpRpc backend;
    private volatile ErlModel erlangModel;

    @Override
    public IErlModel getModel() {
        if (erlangModel == null) {
            erlangModel = new ErlModel();
        }
        if (!erlangModel.isOpen()) {
            try {
                erlangModel.open(null);
            } catch (final CoreException e) {
                ErlLogger.error(e);
            }
        }
        return erlangModel;
    }

    private volatile String stateDir;

    @Override
    public String getStateDir() {
        if (stateDir == null) {
            final Bundle modelPlugin = Platform.getBundle(ModelPlugin.PLUGIN_ID);
            stateDir = Platform.getStateLocation(modelPlugin).toPortableString();
        }
        return stateDir;
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
     * IResource.
     * </p>
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
        if (backend == null) {
            return new NullScannerService();
        }
        return new ErlideScanner(backend);
    }

    @Override
    public SimpleParserService getSimpleParserService() {
        return new ErlideParser(backend);
    }

    @Override
    public ModelSearcherService getModelSearcherService() {
        return new ModelSearcher();
    }

    @Override
    public IProjectConfiguratorFactory getProjectConfiguratorFactory() {
        return ProjectConfiguratorFactory.getDefault();
    }

    @Override
    public boolean isAvailable() {
        return backend != null;
    }

    @Override
    public GenericService getGenericService() {
        return new GenericService() {

            @Override
            public OtpErlangObject call(final String module, final String fun,
                    final int offset, final int length, final String text) {
                try {
                    final OtpErlangObject r1 = backend.call(module, fun, "sii", text,
                            offset, length);
                    return r1;
                } catch (final RpcException e) {
                    return new OtpErlangString("");
                }
            }
        };
    }

    @Override
    public void setInitializationData(final IConfigurationElement config,
            final String propertyName, final Object data) throws CoreException {
        backend = OtpRpcFactory.getOtpRpc();
    }

}

package org.erlide.engine;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Platform;
import org.erlide.model.ErlModelException;
import org.erlide.model.ModelPlugin;
import org.erlide.model.erlang.ErlangBackendToolkit;
import org.erlide.model.erlang.ErlangToolkit;
import org.erlide.model.internal.erlang.ErlideScanner;
import org.erlide.model.internal.erlang.ModelInternalUtils;
import org.erlide.model.internal.root.ErlModel;
import org.erlide.model.root.IErlModel;
import org.erlide.model.services.cleanup.CleanupProvider;
import org.erlide.model.services.cleanup.ErlTidyCleanupProvider;
import org.erlide.model.services.codeassist.ContextAssistService;
import org.erlide.model.services.codeassist.ErlideContextAssist;
import org.erlide.model.services.scanner.ScannerService;
import org.erlide.model.services.search.ErlangXref;
import org.erlide.model.services.search.ErlideDoc;
import org.erlide.model.services.search.ErlideOpen;
import org.erlide.model.services.search.ErlideSearchServer;
import org.erlide.model.services.search.OpenService;
import org.erlide.model.services.search.OtpDocService;
import org.erlide.model.services.search.SearchServerService;
import org.erlide.model.services.search.XrefService;
import org.erlide.model.services.text.ErlideIndent;
import org.erlide.model.services.text.IndentService;
import org.erlide.model.util.ModelUtilService;
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
        return new ErlideOpen(backend);
    }

    @Override
    public OtpDocService getOtpDocService() {
        return new ErlideDoc();
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
    public ScannerService getScannerService() {
        return new ErlideScanner(backend);
    }

    @Override
    public SearchServerService getSearchServerService() {
        return new ErlideSearchServer(backend);
    }

    @Override
    public ModelUtilService getModelUtilService() {
        return new ModelInternalUtils();
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
        return new ErlTidyCleanupProvider(resource);
    }

    @Override
    public ErlangToolkit getToolkit() {
        return new ErlangBackendToolkit(backend);
    }
}

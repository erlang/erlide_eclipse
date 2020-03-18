package org.erlide.engine.internal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.xtend.lib.annotations.Delegate;
import org.erlide.engine.ErlangInitializeParams;
import org.erlide.engine.IErlangEngine;
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
public class ErlangServerProxy implements IErlangEngine, IExecutableExtension {
    @Delegate
    private final IErlangEngine server;

    public ErlangServerProxy() {
        this(new ErlangServerImpl());
    }

    public ErlangServerProxy(final IErlangEngine server) {
        this.server = server;
    }

    @Override
    public void setInitializationData(final IConfigurationElement config,
            final String propertyName, final Object data) throws CoreException {
    }

    @Override
    public CleanupProvider getCleanupProvider() {
        return server.getCleanupProvider();
    }

    @Override
    public CompletionService getCompletionService() {
        return server.getCompletionService();
    }

    @Override
    public EdocExportService getEdocExportService() {
        return server.getEdocExportService();
    }

    @Override
    public IndentService getIndentService() {
        return server.getIndentService();
    }

    @Override
    public IErlModel getModel() {
        return server.getModel();
    }

    @Override
    public ModelFindService getModelFindService() {
        return server.getModelFindService();
    }

    @Override
    public ModelUtilService getModelUtilService() {
        return server.getModelUtilService();
    }

    @Override
    public OpenService getOpenService() {
        return server.getOpenService();
    }

    @Override
    public OtpDocService getOtpDocService() {
        return server.getOtpDocService();
    }

    @Override
    public ProclistService getProclistService() {
        return server.getProclistService();
    }

    @Override
    public ScannerProviderService getScannerProviderService() {
        return server.getScannerProviderService();
    }

    @Override
    public SearchServerService getSearchServerService() {
        return server.getSearchServerService();
    }

    @Override
    public SimpleParserService getSimpleParserService() {
        return server.getSimpleParserService();
    }

    @Override
    public SimpleScannerService getSimpleScannerService() {
        return server.getSimpleScannerService();
    }

    @Override
    public String getStateDir() {
        return server.getStateDir();
    }

    @Override
    public SystemInfoService getSystemInfoService() {
        return server.getSystemInfoService();
    }

    @Override
    public ToggleCommentService getToggleCommentService() {
        return server.getToggleCommentService();
    }

    @Override
    public void initialize(final ErlangInitializeParams params) {
        server.initialize(params);
    }

    @Override
    public boolean isAvailable() {
        return server.isAvailable();
    }

    @Override
    public void shutdown() {
        server.shutdown();
    }
}

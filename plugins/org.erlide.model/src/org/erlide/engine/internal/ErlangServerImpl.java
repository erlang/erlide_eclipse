package org.erlide.engine.internal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.engine.ErlangInitializeParams;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.internal.model.ErlModel;
import org.erlide.engine.internal.model.erlang.ModelFindUtil;
import org.erlide.engine.internal.model.erlang.ModelInternalUtils;
import org.erlide.engine.internal.services.cleanup.ErlTidyCleanupProvider;
import org.erlide.engine.internal.services.codeassist.ErlangCompletionService;
import org.erlide.engine.internal.services.edoc.ErlideEdocExport;
import org.erlide.engine.internal.services.parsing.ErlideParser;
import org.erlide.engine.internal.services.parsing.ErlideScanner;
import org.erlide.engine.internal.services.parsing.ScannerProvider;
import org.erlide.engine.internal.services.proclist.ErlideProclist;
import org.erlide.engine.internal.services.search.ErlideDoc;
import org.erlide.engine.internal.services.search.ErlideOpen;
import org.erlide.engine.internal.services.search.ErlideSearchServer;
import org.erlide.engine.internal.services.text.ErlideIndent;
import org.erlide.engine.model.OtpRpcFactory;
import org.erlide.engine.model.root.IErlModel;
import org.erlide.engine.services.SystemInfoService;
import org.erlide.engine.services.ToggleCommentService;
import org.erlide.engine.services.cleanup.CleanupProvider;
import org.erlide.engine.services.codeassist.CompletionService;
import org.erlide.engine.services.edoc.EdocExportService;
import org.erlide.engine.services.parsing.NullScannerService;
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
import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

@SuppressWarnings("all")
public class ErlangServerImpl implements IErlangEngine {
    private IOtpRpc backend;

    private volatile ErlModel erlangModel;

    private volatile String stateDir;

    @Override
    public void initialize(final ErlangInitializeParams params) {
        backend = OtpRpcFactory.getOtpRpc();
        stateDir = params.getStateDir();
    }

    @Override
    public void shutdown() {
    }

    @Override
    public IErlModel getModel() {
        if (erlangModel == null) {
            final ErlModel _erlModel = new ErlModel();
            erlangModel = _erlModel;
        }
        final boolean _isOpen = erlangModel.isOpen();
        final boolean _not = !_isOpen;
        if (_not) {
            try {
                erlangModel.open(null);
            } catch (final Throwable _t) {
                if (_t instanceof CoreException) {
                    final CoreException e = (CoreException) _t;
                    ErlLogger.error(e);
                } else {
                    throw Exceptions.sneakyThrow(_t);
                }
            }
        }
        return erlangModel;
    }

    @Override
    public String getStateDir() {
        return stateDir;
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
     * Construct a {@link CleanUpProvider} appropriate for a particular IResource.
     * </p>
     */
    @Override
    public CleanupProvider getCleanupProvider() {
        return new ErlTidyCleanupProvider(backend);
    }

    @Override
    public ScannerProviderService getScannerProviderService() {
        return new ScannerProvider(backend);
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
    public CompletionService getCompletionService() {
        return new ErlangCompletionService(backend);
    }

    @Override
    public boolean isAvailable() {
        return backend != null;
    }

    @Override
    public ToggleCommentService getToggleCommentService() {
        final ToggleCommentService _function = (final int offset, final int length,
                final String text) -> {
            try {
                final OtpErlangObject r1 = backend.call("erlide_comment",
                        "toggle_comment", "sii", text, Integer.valueOf(offset),
                        Integer.valueOf(length));
                return r1;
            } catch (final Throwable _t) {
                if (_t instanceof RpcException) {
                    return new OtpErlangString("");
                } else {
                    throw Exceptions.sneakyThrow(_t);
                }
            }
        };
        return _function;
    }

    @Override
    public IndentService getIndentService() {
        return new ErlideIndent(backend);
    }

    @Override
    public OpenService getOpenService() {
        final String _stateDir = getStateDir();
        return new ErlideOpen(backend, _stateDir);
    }

    @Override
    public OtpDocService getOtpDocService() {
        final String _stateDir = getStateDir();
        return new ErlideDoc(backend, _stateDir);
    }

    @Override
    public SystemInfoService getSystemInfoService() {
        return new SystemInfo(backend);
    }
}

package org.ttb.integration;

import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.jinterface.util.ErlLogger;
import org.ttb.integration.mvc.model.TracePattern;

/**
 * Backend which collects traces from all nodes which are being traced.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TtbBackend {

    private final Backend backend = ErlangCore.getBackendManager().getIdeBackend();
    private static final TtbBackend INSTANCE = new TtbBackend();
    private boolean started = false;
    private static final String MODULE = "ttb";
    private static final String FUN_TRACER = "tracer";
    private static final String FUN_STOP = "stop";
    private static final String FUN_P = "p";

    private TtbBackend() {
    }

    public static TtbBackend getInstance() {
        return INSTANCE;
    }

    /**
     * Checks if tracing is started.
     * 
     * @return <code>true</code> if started, <code>false</code> otherwise
     */
    public boolean isStarted() {
        return started;
    }

    /**
     * Starts tracing.
     * 
     * @return <code>true</code> if successful, <code>false</code> otherwise
     */
    public boolean start() {
        if (!started) {
            synchronized (this) {
                if (!started) {
                    RpcResult rpcResult = backend.call_noexception(MODULE, FUN_TRACER, "", new Object[0]);
                    if (rpcResult.isOk()) {
                        rpcResult = backend.call_noexception(MODULE, FUN_P, "aa", "all", "call");
                        started = true;
                    } else {
                        ErlLogger.error("Could not start tracing tool");
                    }
                }
            }
        }
        return started;
    }

    /**
     * Stops tracing tool.
     */
    public void stop() {
        if (started) {
            RpcResult rpcResult = backend.call_noexception(MODULE, FUN_STOP, "", new Object[0]);
            if (rpcResult.isOk()) {
                started = false;
            } else {
                ErlLogger.error("Could not stop tracing tool");
            }
        }
    }

    public void addTracePattern(TracePattern pattern) {

    }

    public void removeTracePattern(TracePattern pattern) {

    }
}

package org.ttb.integration;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.jinterface.util.ErlLogger;
import org.ttb.integration.mvc.model.TracePattern;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;

/**
 * Backend which collects traces from all nodes which are being traced.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TtbBackend {

    private Backend backend;// =
                            // ErlangCore.getBackendManager().getIdeBackend();
                            // private static final TtbBackend INSTANCE =
                            // new TtbBackend();
    private boolean started = false;
    private static final String MODULE = "ttb";
    private static final String FUN_TRACER = "tracer";
    private static final String FUN_STOP = "stop";
    private static final String FUN_P = "p";
    private static final String FUN_TP = "tp";
    private static final String FUN_CTP = "ctp";

    // public TtbBackend(Backend backend) {
    // this.backend = backend;
    // }

    // public static TtbBackend getInstance() {
    // return INSTANCE;
    // }

    /**
     * Checks if tracing is started.
     * 
     * @return <code>true</code> if started, <code>false</code> otherwise
     */
    public boolean isStarted() {
        return started;
    }

    public void setBackend(Backend backend) {
        this.backend = backend;
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
                        ErlLogger.error("Could not start tracing tool: " + rpcResult.getValue());
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
            RpcResult rpcResult = backend.call_noexception(MODULE, FUN_STOP, "x", new OtpErlangList(new OtpErlangAtom("format")));
            System.out.println("stop:\n" + rpcResult.getValue());
            if (rpcResult.isOk()) {
                started = false;
            } else {
                ErlLogger.error("Could not stop tracing tool");
            }
        }
    }

    public void addTracePattern(TracePattern pattern) {
        if (pattern.isEnabled()) {
            RpcResult rpcResult = backend.call_noexception(MODULE, FUN_TP, "aax", pattern.getModuleName(), pattern.getFunctionName(), new OtpErlangList());
            System.out.println("addTracePattern:\n" + rpcResult.getValue());
        }
    }

    public void removeTracePattern(TracePattern pattern) {
        RpcResult rpcResult = backend.call_noexception(MODULE, FUN_CTP, "aa", pattern.getModuleName(), pattern.getFunctionName());
        System.out.println("removeTracePattern:\n" + rpcResult.getValue());
    }
}

package org.ttb.integration;

import java.util.ArrayList;
import java.util.List;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;
import org.ttb.integration.mvc.model.CollectedDataList;
import org.ttb.integration.mvc.model.ITraceNodeObserver;
import org.ttb.integration.mvc.model.TracePattern;

import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * Singleton class wrapping backend that collects traces.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TtbBackend {

    private static final String PROCESS_NAME = "TraceDataCollector";
    private final List<TracePattern> list = new ArrayList<TracePattern>();
    private final List<ITraceNodeObserver> listeners = new ArrayList<ITraceNodeObserver>();
    private Backend backend;
    private static final TtbBackend INSTANCE = new TtbBackend();
    private boolean started;
    private TraceDataCollectorThread traceDataCollectorThread;
    private static final String TTB_MODULE = "ttb";
    private static final String HELPER_MODULE = "ttb_integration";
    private static final String FUN_TRACER = "tracer";
    private static final String FUN_STOP = "stop";
    private static final String FUN_P = "p";
    private static final String FUN_TP = "tp";
    private static final String FUN_TPL = "tpl";
    private static final String FUN_CTP = "ctp";
    private static final String FUN_FORMAT = "format";
    private static final String FUN_START = "start";
    private CollectedDataList collectedData;
    private OtpMbox otpMbox;

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
    public boolean start(Backend backend) {
        if (!started) {
            synchronized (this) {
                if (!started) {
                    try {
                        this.backend = backend;
                        otpMbox = backend.createMbox(PROCESS_NAME);
                        OtpErlangPid pid = otpMbox.self();
                        traceDataCollectorThread = new TraceDataCollectorThread(otpMbox);
                        backend.call(HELPER_MODULE, FUN_START, "x", pid);

                        for (TracePattern tracePattern : list) {
                            if (tracePattern.isEnabled()) {
                                String function = tracePattern.isLocal() ? FUN_TPL : FUN_TP;
                                try {
                                    backend.call(TTB_MODULE, function, "aax", tracePattern.getModuleName(), tracePattern.getFunctionName(), new Object[0]);
                                } catch (BackendException e) {
                                    ErlLogger.error("Could not add pattern: " + e.getMessage());
                                }
                            }
                        }
                        traceDataCollectorThread.start();
                        started = true;
                        for (ITraceNodeObserver listener : listeners) {
                            listener.startTracing();
                        }
                    } catch (BackendException e) {
                        if (otpMbox != null) {
                            otpMbox.close();
                        }
                        ErlLogger.error("Could not start tracing tool: " + e.getMessage());
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
            synchronized (this) {
                if (started) {
                    try {
                        backend.call(HELPER_MODULE, FUN_STOP, "x", traceDataCollectorThread.getOtpMbox().self());
                        collectedData = traceDataCollectorThread.getCollectedData();
                        started = false;
                        for (ITraceNodeObserver listener : listeners) {
                            listener.stopTracing();
                        }
                    } catch (BackendException e) {
                        ErlLogger.error("Could not stop tracing tool: " + e.getMessage());
                    } finally {
                        if (otpMbox != null) {
                            otpMbox.close();
                        }
                    }
                }
            }
        }
    }

    public synchronized void addListener(ITraceNodeObserver listener) {
        listeners.add(listener);
    }

    public synchronized void removeListener(ITraceNodeObserver listener) {
        listeners.remove(listener);
    }

    public Object[] getTracePatternsArray() {
        return list.toArray();
    }

    public synchronized void addTracePattern(TracePattern pattern) {
        list.add(pattern);
        for (ITraceNodeObserver listener : listeners) {
            listener.addPattern(pattern);
        }
    }

    public synchronized void removeTracePattern(TracePattern pattern) {
        list.remove(pattern);
        for (ITraceNodeObserver listener : listeners) {
            listener.removePattern(pattern);
        }
    }

    public synchronized void updateTracePattern(TracePattern tracePattern) {
        for (ITraceNodeObserver listener : listeners) {
            listener.updatePattern(tracePattern);
        }
    }

    public CollectedDataList getCollectedData() {
        return collectedData;
    }
}

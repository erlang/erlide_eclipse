package org.ttb.integration;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;
import org.ttb.integration.mvc.model.CollectedDataList;
import org.ttb.integration.mvc.model.ITraceNodeObserver;
import org.ttb.integration.mvc.model.ITreeNode;
import org.ttb.integration.mvc.model.ProcessOnList;
import org.ttb.integration.mvc.model.TracePattern;

import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * Singleton class wrapping backend that collects traces.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TtbBackend {

    private static final TtbBackend INSTANCE = new TtbBackend();
    private static final String PROCESS_NAME = "TraceDataCollector";
    private static final String TTB_MODULE = "ttbe";
    private static final String HELPER_MODULE = "ttb_integration";
    private static final String FUN_STOP = "stop";
    private static final String FUN_P = "p";
    private static final String FUN_TP = "tp";
    private static final String FUN_TPL = "tpl";
    private static final String FUN_START = "start";

    private final List<TracePattern> list = new ArrayList<TracePattern>();
    private final List<ITraceNodeObserver> listeners = new ArrayList<ITraceNodeObserver>();
    private final Set<ProcessFlag> processFlags = new HashSet<ProcessFlag>();
    private ProcessOnList[] processes;
    private ProcessMode processMode;
    private Backend backend;
    private boolean started;
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
                        backend.call(HELPER_MODULE, FUN_START, "x", pid);

                        // setting process flags
                        if (ProcessMode.BY_PID.equals(processMode)) {
                            // setting flags only for selected processes
                            if (processes != null) {
                                for (ProcessOnList process : processes) {
                                    if (process.isSelected()) {
                                        backend.call(TTB_MODULE, FUN_P, "xx", process.getPid(), createProcessFlagsArray(process.getFlags()));
                                    }
                                }
                            }
                        } else {
                            // setting global flags
                            backend.call(TTB_MODULE, FUN_P, "ax", processMode.toAtom(), createProcessFlagsArray(processFlags));
                        }

                        // setting function trace patterns
                        for (TracePattern tracePattern : list) {
                            if (tracePattern.isEnabled()) {
                                String function = tracePattern.isLocal() ? FUN_TPL : FUN_TP;
                                try {
                                    if (tracePattern.getArity() < 0) {
                                        backend.call(TTB_MODULE, function, "aax", tracePattern.getModuleName(), tracePattern.getFunctionName(), new Object[0]);
                                    } else {
                                        backend.call(TTB_MODULE, function, "aaxx", tracePattern.getModuleName(), tracePattern.getFunctionName(),
                                                new OtpErlangInt(tracePattern.getArity()), new Object[0]);
                                    }
                                } catch (BackendException e) {
                                    ErlLogger.error("Could not add pattern: " + e.getMessage());
                                }
                            }
                        }
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
     * Stops tracing.
     */
    public void stop() {
        if (started) {
            synchronized (this) {
                if (started) {
                    try {
                        backend.call(HELPER_MODULE, FUN_STOP, "x", otpMbox.self());
                        ITreeNode root = new TraceDataHandler(otpMbox).getData();
                        CollectedDataList.getInstance().addData(root);

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

    private OtpErlangObject[] createProcessFlagsArray(Set<ProcessFlag> set) {
        OtpErlangObject[] array = new OtpErlangObject[set.size()];
        Iterator<ProcessFlag> iterator = set.iterator();
        int i = 0;
        while (iterator.hasNext()) {
            array[i++] = iterator.next().toAtom();
        }
        return array;
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

    public void addProcessFlag(ProcessFlag flag) {
        processFlags.add(flag);
    }

    public void removeProcessFlag(ProcessFlag flag) {
        processFlags.remove(flag);
    }

    public void removeAllProcessFlag() {
        processFlags.clear();
    }

    public ProcessMode getProcessMode() {
        return processMode;
    }

    public void setProcessMode(ProcessMode processMode) {
        this.processMode = processMode;
    }

    public void setProcesses(ProcessOnList[] processes) {
        this.processes = processes;
    }
}

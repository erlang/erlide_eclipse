package org.ttb.integration;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.events.EventHandler;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.ttb.integration.mvc.model.CollectedDataList;
import org.ttb.integration.mvc.model.ITraceNodeObserver;
import org.ttb.integration.mvc.model.ProcessOnList;
import org.ttb.integration.mvc.model.TracePattern;
import org.ttb.integration.mvc.model.treenodes.ITreeNode;
import org.ttb.integration.mvc.model.treenodes.TracingResultsNode;

import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * Singleton class used for communication with Erlang nodes.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TtbBackend {

    private static final TtbBackend INSTANCE = new TtbBackend();
    private static final String FUN_STOP = "stop";
    private static final String FUN_P = "p";
    private static final String FUN_TP = "tp";
    private static final String FUN_TPL = "tpl";
    private static final String FUN_START = "start";
    private static final String FUN_LOAD = "load";

    private final List<TracePattern> list = new ArrayList<TracePattern>();
    private final List<ITraceNodeObserver> listeners = new ArrayList<ITraceNodeObserver>();
    private final Set<ProcessFlag> processFlags = new HashSet<ProcessFlag>();
    private ProcessOnList[] processes;
    private ProcessMode processMode;
    private Backend backend;
    private boolean tracing;
    private TraceEventHandler handler;

    // private ITreeNode rootNode;

    private TtbBackend() {
    }

    public static TtbBackend getInstance() {
        return INSTANCE;
    }

    private class TraceEventHandler extends EventHandler {

        private final TraceDataHandler handler = new TraceDataHandler();
        private TracingResultsNode rootNode;
        private boolean firstTrace = true;

        @Override
        protected void doHandleMsg(OtpErlangObject msg) throws Exception {
            OtpErlangObject message = getStandardEvent(msg, "trace_event");
            if (message != null) {
                if (handler.isTracingFinished(message)) {
                    if (rootNode != null) {
                        rootNode.setEndDate(handler.getLastTraceDate());
                        rootNode.generateLabel(handler.getRootDateFormatter());
                    }
                    finishTracing();
                } else if (handler.isLoadingFinished(message)) {
                    if (rootNode != null) {
                        rootNode.setEndDate(handler.getLastTraceDate());
                        rootNode.generateLabel(handler.getRootDateFormatter());
                    }
                    finishLoading();
                    // TODO handle error which may occur during loading
                } else {
                    ITreeNode newNode = handler.getData(message);
                    if (newNode != null) {
                        if (firstTrace) {
                            firstTrace = false;
                            rootNode = handler.createRoot();
                            CollectedDataList.getInstance().addData(rootNode);
                        }
                        rootNode.addChildren(newNode);
                        for (ITraceNodeObserver listener : listeners) {
                            listener.receivedTraceData();
                        }
                    }
                }
            }
        }
    }

    /**
     * Checks if tracing is started.
     * 
     * @return <code>true</code> if started, <code>false</code> otherwise
     */
    public boolean isStarted() {
        return tracing;
    }

    /**
     * Starts tracing.
     * 
     * @return <code>true</code> if successful, <code>false</code> otherwise
     */
    public boolean start(Backend backend) {
        if (!tracing) {
            synchronized (this) {
                if (!tracing) {
                    try {
                        this.backend = backend;
                        handler = new TraceEventHandler();
                        backend.getEventDaemon().addHandler(handler);

                        backend.call(Constants.ERLANG_HELPER_MODULE, FUN_START, "", new Object[0]);

                        // setting process flags
                        if (ProcessMode.BY_PID.equals(processMode)) {
                            // setting flags only for selected processes
                            if (processes != null) {
                                for (ProcessOnList process : processes) {
                                    if (process.isSelected()) {
                                        backend.call(Constants.TTB_MODULE, FUN_P, "xx", process.getPid(), createProcessFlagsArray(process.getFlags()));
                                    }
                                }
                            }
                        } else {
                            // setting global flags
                            backend.call(Constants.TTB_MODULE, FUN_P, "ax", processMode.toAtom(), createProcessFlagsArray(processFlags));
                        }

                        // setting function trace patterns
                        for (TracePattern tracePattern : list) {
                            if (tracePattern.isEnabled()) {
                                String function = tracePattern.isLocal() ? FUN_TPL : FUN_TP;
                                try {
                                    OtpErlangObject matchSpec = null;
                                    if (tracePattern.getMatchSpec().getMsObject() != null) {
                                        matchSpec = tracePattern.getMatchSpec().getMsObject();
                                    } else {
                                        matchSpec = new OtpErlangList();
                                    }
                                    if (tracePattern.getArity() < 0) {
                                        backend.call(Constants.TTB_MODULE, function, "aax", tracePattern.getModuleName(), tracePattern.getFunctionName(),
                                                matchSpec);
                                    } else {
                                        backend.call(Constants.TTB_MODULE, function, "aaxx", tracePattern.getModuleName(), tracePattern.getFunctionName(),
                                                new OtpErlangInt(tracePattern.getArity()), matchSpec);
                                    }
                                } catch (BackendException e) {
                                    ErlLogger.error("Could not add pattern: " + e.getMessage());
                                }
                            }
                        }
                        tracing = true;
                        for (ITraceNodeObserver listener : listeners) {
                            listener.startTracing();
                        }
                    } catch (BackendException e) {
                        ErlLogger.error("Could not start tracing tool: " + e.getMessage());
                    }
                }
            }
        }
        return tracing;
    }

    /**
     * Stops tracing.
     */
    public void stop() {
        if (tracing) {
            synchronized (this) {
                if (tracing) {
                    try {
                        backend.call(Constants.ERLANG_HELPER_MODULE, FUN_STOP, "");
                    } catch (BackendException e) {
                        ErlLogger.error("Could not stop tracing tool: " + e.getMessage());
                        // TODO what if exception is thrown? - UI is locked
                    } finally {
                    }
                }
            }
        }
    }

    public void loadData(String path) {
        if (!tracing) {
            synchronized (this) {
                if (!tracing) {
                    try {
                        handler = new TraceEventHandler();
                        BackendManager.getDefault().getIdeBackend().getEventDaemon().addHandler(handler);
                        BackendManager.getDefault().getIdeBackend().call(Constants.ERLANG_HELPER_MODULE, FUN_LOAD, "s", new OtpErlangString(path));
                    } catch (BackendException e) {
                        ErlLogger.error("Could not load data: " + e.getMessage());
                        // TODO what if exception is thrown? - UI is locked
                        // forever
                    }
                }
            }
        }
    }

    private void finishTracing() {
        backend.getEventDaemon().removeHandler(handler);
        tracing = false;
        for (ITraceNodeObserver listener : listeners) {
            listener.stopTracing();
        }
    }

    private void finishLoading() {
        BackendManager.getDefault().getIdeBackend().getEventDaemon().removeHandler(handler);
        for (ITraceNodeObserver listener : listeners) {
            listener.stopLoading();
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

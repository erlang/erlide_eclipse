package org.ttb.integration;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.events.EventHandler;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BackendManager.BackendOptions;
import org.erlide.runtime.backend.ErtsProcess;
import org.erlide.runtime.launch.ErlLaunchAttributes;
import org.ttb.integration.mvc.model.CollectedDataList;
import org.ttb.integration.mvc.model.ITraceNodeObserver;
import org.ttb.integration.mvc.model.TracePattern;
import org.ttb.integration.mvc.model.TracedNode;
import org.ttb.integration.mvc.model.TracedProcess;
import org.ttb.integration.mvc.model.treenodes.ITreeNode;
import org.ttb.integration.mvc.model.treenodes.TracingResultsNode;
import org.ttb.integration.preferences.PreferenceNames;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Singleton class used for communication with trace node.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TraceBackend {

    private static final TraceBackend INSTANCE = new TraceBackend();
    private static final String EVENT_NAME = "trace_event";
    private static final String FUN_STOP = "stop";
    private static final String FUN_P = "p";
    private static final String FUN_TP = "tp";
    private static final String FUN_TPL = "tpl";
    private static final String FUN_START = "start";
    private static final String FUN_LOAD = "load";

    private final Set<TracePattern> tracePatterns = new LinkedHashSet<TracePattern>();
    private final Set<TracedNode> tracedNodes = new LinkedHashSet<TracedNode>();
    private final List<ITraceNodeObserver> listeners = new ArrayList<ITraceNodeObserver>();
    private final Set<ProcessFlag> processFlags = new HashSet<ProcessFlag>();
    private TracedProcess[] processes;
    private ProcessMode processMode;
    private Backend tracerBackend;
    private boolean tracing;
    private boolean loading;
    private TraceEventHandler handler;
    private List<String> activatedNodes;
    private Set<String> notActivatedNodes;
    private Object errorObject;

    private TraceBackend() {
    }

    public static TraceBackend getInstance() {
        return INSTANCE;
    }

    private class TraceEventHandler extends EventHandler {

        private final TraceDataHandler handler = new TraceDataHandler();
        private TracingResultsNode rootNode;
        private boolean firstTrace = true;

        @Override
        protected void doHandleMsg(OtpErlangObject msg) throws Exception {
            OtpErlangObject message = getStandardEvent(msg, EVENT_NAME);
            if (message != null) {
                OtpErlangObject errorReason = null;
                // System.out.println("message: " + message);
                if (handler.isTracingFinished(message)) {
                    if (rootNode != null) {
                        rootNode.setEndDate(handler.getLastTraceDate());
                        rootNode.generateLabel(handler.getRootDateFormatter());
                    }
                    finishTracing(TracingStatus.OK);
                } else if ((errorReason = handler.getErrorReson(message)) != null) {
                    errorObject = errorReason;
                    finishTracing(TracingStatus.ERROR);
                } else {
                    ITreeNode newNode = handler.getData(message);
                    if (newNode != null) {
                        if (firstTrace) {
                            firstTrace = false;
                            rootNode = handler.createRoot();
                            CollectedDataList.getInstance().addData(rootNode);
                        }
                        rootNode.addChildren(newNode);
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
     * Checks if trace results are being loaded (i.e. backend receives results).
     * 
     * @return <code>true</code> if loading in progress, <code>false</code>
     *         otherwise
     */
    public boolean isLoading() {
        return loading;
    }

    /**
     * Starts tracing given nodes.
     * 
     * @return <code>true</code> if successful, <code>false</code> otherwise
     */
    public TracingStatus start() {
        TracingStatus status = TracingStatus.OK;
        if (!tracing) {
            synchronized (this) {
                if (!tracing) {
                    try {
                        tracing = true;
                        getBackend(true);
                        handler = new TraceEventHandler();
                        tracerBackend.getEventDaemon().addHandler(handler);

                        // list of nodes being traced
                        List<OtpErlangObject> erlangObjects = new ArrayList<OtpErlangObject>();
                        notActivatedNodes = new HashSet<String>();
                        for (TracedNode tracedNode : tracedNodes) {
                            if (tracedNode.isEnabled()) {
                                OtpErlangAtom name = new OtpErlangAtom(tracedNode.getNodeName());
                                OtpErlangAtom cookie = new OtpErlangAtom(tracedNode.getCookie());

                                erlangObjects.add(new OtpErlangTuple(new OtpErlangObject[] { name, cookie }));
                                notActivatedNodes.add(tracedNode.getNodeName());
                            }
                        }
                        OtpErlangList nodes = new OtpErlangList(erlangObjects.toArray(new OtpErlangObject[erlangObjects.size()]));

                        // net tick time
                        int tickTimeValue = Activator.getDefault().getPreferenceStore().getInt(PreferenceNames.TICK_TIME);
                        OtpErlangInt netTickTime = new OtpErlangInt(tickTimeValue);

                        OtpErlangObject callResult = tracerBackend.call(Constants.ERLANG_HELPER_MODULE, FUN_START, "xsi", nodes, Constants.OUTPUT_FILE,
                                netTickTime);
                        status = processResult(callResult);

                        if (TracingStatus.OK.equals(status) || TracingStatus.NOT_ALL_NODES_ACTIVATED.equals(status)) {
                            setProcessFlags();
                            setFunctionTracePatterns();
                            for (ITraceNodeObserver listener : listeners) {
                                try {
                                    listener.startTracing();
                                } catch (Exception e) {
                                    ErlLogger.error(e);
                                }
                            }
                        } else {
                            tracing = false;
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                        ErlLogger.error("Could not start tracing tool: " + e.getMessage());
                        status = TracingStatus.EXCEPTION_THROWN;
                        errorObject = e;
                        tracing = false;
                    }
                }
            }
        }
        return status;
    }

    private TracingStatus processResult(OtpErlangObject callResult) {
        OtpErlangTuple tuple = (OtpErlangTuple) callResult;
        if (((OtpErlangAtom) tuple.elementAt(0)).atomValue().equals("error")) {
            errorObject = tuple.elementAt(1);
            return TracingStatus.ERROR;
        } else {
            OtpErlangList nodeNames = (OtpErlangList) tuple.elementAt(1);
            activatedNodes = new ArrayList<String>();
            for (OtpErlangObject nodeName : nodeNames.elements()) {
                String nodeNameString = ((OtpErlangAtom) nodeName).atomValue();
                activatedNodes.add(nodeNameString);
                notActivatedNodes.remove(nodeNameString);
            }
            if (activatedNodes.size() == 0)
                return TracingStatus.NO_ACTIVATED_NODES;
            else if (notActivatedNodes.size() != 0)
                return TracingStatus.NOT_ALL_NODES_ACTIVATED;
            else
                return TracingStatus.OK;
        }
    }

    private void setFunctionTracePatterns() {
        for (TracePattern tracePattern : tracePatterns) {
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
                        tracerBackend.call(Constants.TTB_MODULE, function, "aax", tracePattern.getModuleName(), tracePattern.getFunctionName(), matchSpec);
                    } else {
                        tracerBackend.call(Constants.TTB_MODULE, function, "aaxx", tracePattern.getModuleName(), tracePattern.getFunctionName(),
                                new OtpErlangInt(tracePattern.getArity()), matchSpec);
                    }
                } catch (BackendException e) {
                    ErlLogger.error("Could not add pattern: " + e.getMessage());
                }
            }
        }
    }

    private void setProcessFlags() throws BackendException {
        if (ProcessMode.BY_PID.equals(processMode)) {
            // setting flags only for selected processes
            if (processes != null) {
                for (TracedProcess process : processes) {
                    if (process.isSelected()) {
                        tracerBackend.call(Constants.TTB_MODULE, FUN_P, "xx", process.getPid(), createProcessFlagsArray(process.getFlags()));
                    }
                }
            }
        } else {
            // setting global flags
            tracerBackend.call(Constants.TTB_MODULE, FUN_P, "ax", processMode.toAtom(), createProcessFlagsArray(processFlags));
        }
    }

    /**
     * Stops tracing.
     */
    public void stop() {
        if (tracing && !loading) {
            synchronized (this) {
                if (tracing && !loading) {
                    try {
                        loading = true;
                        tracerBackend.call(Constants.ERLANG_HELPER_MODULE, FUN_STOP, "");
                    } catch (BackendException e) {
                        ErlLogger.error("Could not stop tracing tool: " + e.getMessage());
                        errorObject = e;
                        finishTracing(TracingStatus.EXCEPTION_THROWN);
                    }
                }
            }
        }
    }

    public void loadData(String path) {
        if (!tracing && !loading) {
            synchronized (this) {
                if (!tracing && !loading) {
                    try {
                        loading = true;
                        handler = new TraceEventHandler();
                        getBackend(true);
                        tracerBackend.getEventDaemon().addHandler(handler);
                        tracerBackend.call(Constants.ERLANG_HELPER_MODULE, FUN_LOAD, "s", new OtpErlangString(path));
                    } catch (BackendException e) {
                        ErlLogger.error(e);
                        errorObject = e;
                        finishTracing(TracingStatus.EXCEPTION_THROWN);
                    }
                }
            }
        }
    }

    /**
     * Returns backend used for tracing. If this backend does not exist it can
     * be created.
     * 
     * @param create
     *            if backend should be created when it does not exist
     * @return backend
     */
    public Backend getBackend(boolean create) {
        if (tracerBackend == null && create) {
            tracerBackend = createBackend();
        }
        return tracerBackend;
    }

    /**
     * Performs actions after loading trace data.
     * 
     * @param status
     *            status
     */
    private void finishTracing(TracingStatus status) {
        tracerBackend.getEventDaemon().removeHandler(handler);
        for (ITraceNodeObserver listener : listeners) {
            try {
                listener.finishLoadingFile(status);
            } catch (Exception e) {
                ErlLogger.error(e);
            }
        }
        loading = false;
        tracing = false;
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

    public void loadTracePatterns(TracePattern[] patterns) {
        tracePatterns.clear();
        tracePatterns.addAll(Arrays.asList(patterns));
    }

    public Object[] getTracePatternsArray() {
        return tracePatterns.toArray();
    }

    public synchronized void addTracePattern(TracePattern pattern) {
        tracePatterns.add(pattern);
    }

    public synchronized void removeTracePattern(TracePattern pattern) {
        tracePatterns.remove(pattern);
    }

    public void loadTracedNodes(TracedNode[] nodes) {
        tracedNodes.clear();
        tracedNodes.addAll(Arrays.asList(nodes));
    }

    public Object[] getTracedNodesArray() {
        return tracedNodes.toArray();
    }

    public synchronized void addTracedNode(TracedNode tracedNode) {
        tracedNodes.add(tracedNode);
    }

    public synchronized void removeTracedNode(TracedNode tracedNode) {
        tracedNodes.remove(tracedNode);
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

    public void setProcesses(TracedProcess[] processes) {
        this.processes = processes;
    }

    public TracedProcess[] getProcesses() {
        return processes;
    }

    public List<String> getActivatedNodes() {
        return activatedNodes;
    }

    public Set<String> getNotActivatedNodes() {
        return notActivatedNodes;
    }

    /**
     * Returns object that describes last error (e.g. thrown exception).
     * 
     * @return error details
     */
    public Object getErrorObject() {
        return errorObject;
    }

    private Backend createBackend() {
        final RuntimeInfo info = RuntimeInfo.copy(ErlangCore.getRuntimeInfoManager().getErlideRuntime(), false);
        String nodeName = Activator.getDefault().getPreferenceStore().getString(PreferenceNames.NODE_NAME);

        if (info != null) {
            try {
                info.setNodeName(nodeName);
                info.setStartShell(false);
                EnumSet<BackendOptions> options = EnumSet.of(BackendOptions.AUTOSTART, BackendOptions.NO_CONSOLE);

                ILaunchConfiguration launchConfig = getLaunchConfiguration(info, options);
                launchConfig.launch(ILaunchManager.RUN_MODE, new NullProgressMonitor(), false, false);
                return BackendManager.getDefault().getByName(nodeName);
            } catch (Exception e) {
                ErlLogger.error(e);
            }
        }
        return null;
    }

    private ILaunchConfiguration getLaunchConfiguration(RuntimeInfo info, Set<BackendOptions> options) {
        ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
        ILaunchConfigurationType type = manager.getLaunchConfigurationType(ErtsProcess.CONFIGURATION_TYPE_INTERNAL);
        ILaunchConfigurationWorkingCopy workingCopy;
        try {
            workingCopy = type.newInstance(null, "internal " + info.getNodeName());
            workingCopy.setAttribute(DebugPlugin.ATTR_CONSOLE_ENCODING, "ISO-8859-1");
            workingCopy.setAttribute(ErlLaunchAttributes.NODE_NAME, info.getNodeName());
            workingCopy.setAttribute(ErlLaunchAttributes.RUNTIME_NAME, info.getName());
            workingCopy.setAttribute(ErlLaunchAttributes.COOKIE, info.getCookie());
            workingCopy.setAttribute(ErlLaunchAttributes.CONSOLE, !options.contains(BackendOptions.NO_CONSOLE));
            workingCopy.setAttribute(ErlLaunchAttributes.INTERNAL, options.contains(BackendOptions.INTERNAL));
            workingCopy.setAttribute(ErlLaunchAttributes.USE_LONG_NAME, false);
            return workingCopy.doSave();
        } catch (CoreException e) {
            e.printStackTrace();
            return null;
        }
    }
}

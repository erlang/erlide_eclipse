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
import org.ttb.integration.mvc.model.ProcessOnList;
import org.ttb.integration.mvc.model.TracePattern;
import org.ttb.integration.mvc.model.treenodes.ITreeNode;
import org.ttb.integration.mvc.model.treenodes.TracingResultsNode;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * Singleton class used for communication with Erlang nodes for tracing
 * purposes.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TtbBackend {

    private static final TtbBackend INSTANCE = new TtbBackend();
    private static final String NODE_NAME = "tracing";
    private static final String FUN_STOP = "stop";
    private static final String FUN_P = "p";
    private static final String FUN_TP = "tp";
    private static final String FUN_TPL = "tpl";
    private static final String FUN_START = "start";
    private static final String FUN_LOAD = "load";

    private final Set<TracePattern> tracePatterns = new LinkedHashSet<TracePattern>();
    private final List<ITraceNodeObserver> listeners = new ArrayList<ITraceNodeObserver>();
    private final Set<ProcessFlag> processFlags = new HashSet<ProcessFlag>();
    private ProcessOnList[] processes;
    private ProcessMode processMode;
    private Backend tracerBackend;
    private boolean tracing;
    private TraceEventHandler handler;

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
                // System.out.println("message: " + message);
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
     * Starts tracing given nodes.
     * 
     * @param backends
     *            nodes for tracing
     * 
     * @return <code>true</code> if successful, <code>false</code> otherwise
     */
    public boolean start(List<Backend> backends) {
        if (!tracing) {
            synchronized (this) {
                if (!tracing) {
                    try {

                        if (tracerBackend == null) {
                            tracerBackend = createBackend(NODE_NAME);
                        }

                        handler = new TraceEventHandler();
                        tracerBackend.getEventDaemon().addHandler(handler);

                        // list of nodes being traced
                        List<OtpErlangObject> erlangObjects = new ArrayList<OtpErlangObject>();
                        for (Backend backend : backends) {
                            erlangObjects.add(new OtpErlangAtom(backend.getPeer()));
                        }
                        OtpErlangList nodes = new OtpErlangList(erlangObjects.toArray(new OtpErlangObject[erlangObjects.size()]));

                        tracerBackend.call(Constants.ERLANG_HELPER_MODULE, FUN_START, "xs", nodes, Constants.OUTPUT_FILE);

                        // setting process flags
                        if (ProcessMode.BY_PID.equals(processMode)) {
                            // setting flags only for selected processes
                            if (processes != null) {
                                for (ProcessOnList process : processes) {
                                    if (process.isSelected()) {
                                        tracerBackend.call(Constants.TTB_MODULE, FUN_P, "xx", process.getPid(), createProcessFlagsArray(process.getFlags()));
                                    }
                                }
                            }
                        } else {
                            // setting global flags
                            tracerBackend.call(Constants.TTB_MODULE, FUN_P, "ax", processMode.toAtom(), createProcessFlagsArray(processFlags));
                        }

                        // setting function trace patterns
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
                                        tracerBackend.call(Constants.TTB_MODULE, function, "aax", tracePattern.getModuleName(), tracePattern.getFunctionName(),
                                                matchSpec);
                                    } else {
                                        tracerBackend.call(Constants.TTB_MODULE, function, "aaxx", tracePattern.getModuleName(),
                                                tracePattern.getFunctionName(), new OtpErlangInt(tracePattern.getArity()), matchSpec);
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
                        tracerBackend.call(Constants.ERLANG_HELPER_MODULE, FUN_STOP, "");
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
        tracerBackend.getEventDaemon().removeHandler(handler);
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

    public void loadTracePatterns(TracePattern[] patterns) {
        tracePatterns.clear();
        tracePatterns.addAll(Arrays.asList(patterns));
        for (ITraceNodeObserver listener : listeners) {
            listener.loadPatterns();
        }
    }

    public Object[] getTracePatternsArray() {
        return tracePatterns.toArray();
    }

    public synchronized void addTracePattern(TracePattern pattern) {
        if (!tracePatterns.contains(pattern)) {
            tracePatterns.add(pattern);
            for (ITraceNodeObserver listener : listeners) {
                listener.addPattern(pattern);
            }
        }
    }

    public synchronized void removeTracePattern(TracePattern pattern) {
        tracePatterns.remove(pattern);
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

    private Backend createBackend(String name) {
        final RuntimeInfo info = RuntimeInfo.copy(ErlangCore.getRuntimeInfoManager().getErlideRuntime(), false);
        if (info != null) {
            try {
                info.setNodeName(name);
                EnumSet<BackendOptions> options = EnumSet.of(BackendOptions.AUTOSTART, BackendOptions.NO_CONSOLE);

                ILaunchConfiguration launchConfig = getLaunchConfiguration(info, options);
                launchConfig.launch(ILaunchManager.RUN_MODE, new NullProgressMonitor(), false, false);
                return BackendManager.getDefault().getByName(name);
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

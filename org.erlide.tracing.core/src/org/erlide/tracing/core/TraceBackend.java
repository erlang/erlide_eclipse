package org.erlide.tracing.core;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendData;
import org.erlide.backend.IBackend;
import org.erlide.backend.events.ErlangEventHandler;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.tracing.core.mvc.model.TraceCollections;
import org.erlide.tracing.core.mvc.model.TracePattern;
import org.erlide.tracing.core.mvc.model.TracedNode;
import org.erlide.tracing.core.mvc.model.TracedProcess;
import org.erlide.tracing.core.mvc.model.treenodes.ITreeNode;
import org.erlide.tracing.core.mvc.model.treenodes.TracingResultsNode;
import org.erlide.tracing.core.preferences.PreferenceNames;
import org.erlide.tracing.core.utils.TraceDataHandler;
import org.osgi.service.event.Event;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
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
	private static final String FUN_FILE_INFO = "get_file_info";
	private static final String FUN_LOAD = "load";

	private final Set<TracePattern> tracePatterns = new LinkedHashSet<TracePattern>();
	private final Set<TracedNode> tracedNodes = new LinkedHashSet<TracedNode>();
	private final List<ITraceNodeObserver> listeners = new ArrayList<ITraceNodeObserver>();
	private final Set<ProcessFlag> processFlags = new HashSet<ProcessFlag>();
	private TracedProcess[] processes;
	private ProcessMode processMode;
	private IBackend tracerBackend;
	private boolean tracing;
	private boolean loading;

	/**
	 * <code>true</code> when loading only information about file that contains
	 * tracing results, <code>false</code> when loading traces from file
	 */
	private boolean loadingFileInfo;
	private TraceEventHandler handler;
	private List<String> activatedNodes;
	private Set<String> notActivatedNodes;
	private Object errorObject;
	private long startIndex;
	private TracingResultsNode activeResultSet;

	private TraceBackend() {
	}

	public static TraceBackend getInstance() {
		return INSTANCE;
	}

	private class TraceEventHandler extends ErlangEventHandler {

		public TraceEventHandler(final IBackend backend) {
			super(EVENT_NAME, backend);
		}

		private final TraceDataHandler dataHandler = new TraceDataHandler();
		private boolean firstTrace = true;

		@Override
		public void handleEvent(final Event event) {
			final OtpErlangObject message = (OtpErlangObject) event
					.getProperty("DATA");
			if (message != null) {
				OtpErlangObject errorReason = null;
				// System.out.println("data: " + data);
				if (dataHandler.isTracingFinished(message)) {
					finishLoading(firstTrace ? TracingStatus.EMPTY
							: TracingStatus.OK);
				} else if ((errorReason = dataHandler.getErrorReson(message)) != null) {
					errorObject = errorReason;
					finishLoading(TracingStatus.ERROR);
				} else {
					final ITreeNode newNode = dataHandler.getData(message);
					if (newNode != null) {
						firstTrace = false;
						if (!loadingFileInfo) {
							TraceCollections.getTracesList().add(newNode);
						} else {
							TraceCollections.getFilesList().add(newNode);
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
						loadingFileInfo = true;
						handler = new TraceEventHandler(tracerBackend);
						handler.register();

						// list of nodes being traced
						final List<OtpErlangObject> erlangObjects = new ArrayList<OtpErlangObject>();
						notActivatedNodes = new HashSet<String>();
						for (final TracedNode tracedNode : tracedNodes) {
							if (tracedNode.isEnabled()) {
								final OtpErlangAtom name = new OtpErlangAtom(
										tracedNode.getNodeName());
								final OtpErlangAtom cookie = new OtpErlangAtom(
										tracedNode.getCookie());

								erlangObjects
										.add(new OtpErlangTuple(
												new OtpErlangObject[] { name,
														cookie }));
								notActivatedNodes.add(tracedNode.getNodeName());
							}
						}
						final OtpErlangList nodes = new OtpErlangList(
								erlangObjects
										.toArray(new OtpErlangObject[erlangObjects
												.size()]));

						// net tick time
						final int tickTimeValue = Activator.getDefault()
								.getPreferenceStore()
								.getInt(PreferenceNames.TICK_TIME);
						final OtpErlangInt netTickTime = new OtpErlangInt(
								tickTimeValue);

						final OtpErlangObject callResult = tracerBackend.call(
								Constants.ERLANG_HELPER_MODULE, FUN_START,
								"xsi", nodes, Constants.OUTPUT_FILE,
								netTickTime);
						status = processResult(callResult);

						if (TracingStatus.OK.equals(status)
								|| TracingStatus.NOT_ALL_NODES_ACTIVATED
										.equals(status)) {
							setProcessFlags();
							setFunctionTracePatterns();
							for (final ITraceNodeObserver listener : listeners) {
								try {
									listener.startTracing();
								} catch (final Exception e) {
									ErlLogger.error(e);
								}
							}
						} else {
							tracing = false;
						}
					} catch (final Exception e) {
						e.printStackTrace();
						ErlLogger.error("Could not start tracing tool: "
								+ e.getMessage());
						status = TracingStatus.EXCEPTION_THROWN;
						errorObject = e;
						tracing = false;
					}
				}
			}
		}
		return status;
	}

	private TracingStatus processResult(final OtpErlangObject callResult) {
		final OtpErlangTuple tuple = (OtpErlangTuple) callResult;
		if (((OtpErlangAtom) tuple.elementAt(0)).atomValue().equals("error")) {
			errorObject = tuple.elementAt(1);
			return TracingStatus.ERROR;
		} else {
			final OtpErlangList nodeNames = (OtpErlangList) tuple.elementAt(1);
			activatedNodes = new ArrayList<String>();
			for (final OtpErlangObject nodeName : nodeNames) {
				final String nodeNameString = ((OtpErlangAtom) nodeName)
						.atomValue();
				activatedNodes.add(nodeNameString);
				notActivatedNodes.remove(nodeNameString);
			}
			if (activatedNodes.size() == 0) {
				return TracingStatus.NO_ACTIVATED_NODES;
			} else if (notActivatedNodes.size() != 0) {
				return TracingStatus.NOT_ALL_NODES_ACTIVATED;
			} else {
				return TracingStatus.OK;
			}
		}
	}

	private void setFunctionTracePatterns() {
		for (final TracePattern tracePattern : tracePatterns) {
			if (tracePattern.isEnabled()) {
				final String function = tracePattern.isLocal() ? FUN_TPL
						: FUN_TP;
				try {
					OtpErlangObject matchSpec = null;
					if (tracePattern.getMatchSpec().getMsObject() != null) {
						matchSpec = tracePattern.getMatchSpec().getMsObject();
					} else {
						matchSpec = new OtpErlangList();
					}
					if (tracePattern.getArity() < 0) {
						tracerBackend.call(Constants.TTB_MODULE, function,
								"aax", tracePattern.getModuleName(),
								tracePattern.getFunctionName(), matchSpec);
					} else {
						tracerBackend.call(Constants.TTB_MODULE, function,
								"aaxx", tracePattern.getModuleName(),
								tracePattern.getFunctionName(),
								new OtpErlangInt(tracePattern.getArity()),
								matchSpec);
					}
				} catch (final RpcException e) {
					ErlLogger.error("Could not add pattern: " + e.getMessage());
				}
			}
		}
	}

	private void setProcessFlags() throws RpcException {
		if (ProcessMode.BY_PID.equals(processMode)) {
			// setting flags only for selected processes
			if (processes != null) {
				for (final TracedProcess process : processes) {
					if (process.isSelected()) {
						tracerBackend.call(Constants.TTB_MODULE, FUN_P, "xx",
								process.getPid(),
								createProcessFlagsArray(process.getFlags()));
					}
				}
			}
		} else {
			// setting global flags
			tracerBackend
					.call(Constants.TTB_MODULE, FUN_P, "ax",
							processMode.toAtom(),
							createProcessFlagsArray(processFlags));
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
						tracerBackend.call(Constants.ERLANG_HELPER_MODULE,
								FUN_STOP, "");
					} catch (final RpcException e) {
						ErlLogger.error("Could not stop tracing tool: "
								+ e.getMessage());
						errorObject = e;
						finishLoading(TracingStatus.EXCEPTION_THROWN);
					}
				}
			}
		}
	}

	/**
	 * Loads information about given file.
	 * 
	 * @param path
	 *            path
	 */
	public void loadFile(final String path) {
		if (!tracing && !loading) {
			synchronized (this) {
				if (!tracing && !loading) {
					try {
						loading = true;
						loadingFileInfo = true;
						handler = new TraceEventHandler(tracerBackend);
						getBackend(true);
						handler.register();
						tracerBackend.call(Constants.ERLANG_HELPER_MODULE,
								FUN_FILE_INFO, "s", new OtpErlangString(path));
					} catch (final RpcException e) {
						ErlLogger.error(e);
						errorObject = e;
						finishLoading(TracingStatus.EXCEPTION_THROWN);
					}
				}
			}
		}
	}

	/**
	 * Loads traces from active result set (
	 * {@link #setActiveResultSet(TracingResultsNode)}). Index of last trace
	 * which will be loaded is <code>max(number_of_traces, endIndex)</code>.
	 * 
	 * @param theStartIndex
	 *            number of first trace
	 * @param endIndex
	 *            number of last trace
	 */
	public void loadDataFromFile(final long theStartIndex, final long endIndex) {
		if (!tracing && !loading) {
			synchronized (this) {
				if (!tracing && !loading) {
					try {
						loading = true;
						loadingFileInfo = false;
						startIndex = theStartIndex;
						handler = new TraceEventHandler(tracerBackend);
						getBackend(true);
						TraceCollections.getTracesList().clear();
						handler.register();
						final OtpErlangLong start = new OtpErlangLong(
								theStartIndex);
						final OtpErlangLong stop = new OtpErlangLong(endIndex);
						tracerBackend.call(Constants.ERLANG_HELPER_MODULE,
								FUN_LOAD, "sii", new OtpErlangString(
										activeResultSet.getFileName()), start,
								stop);
					} catch (final RpcException e) {
						ErlLogger.error(e);
						errorObject = e;
						finishLoading(TracingStatus.EXCEPTION_THROWN);
					}
				}
			}
		}
	}

	/**
	 * Removes tracing results from list.
	 */
	public synchronized void clearTraceLists() {
		activeResultSet = null;
		TraceCollections.getFilesList().clear();
		TraceCollections.getTracesList().clear();
		for (final ITraceNodeObserver listener : listeners) {
			try {
				listener.removeFile();
			} catch (final Exception e) {
				ErlLogger.error(e);
			}
		}
	}

	/**
	 * Removes selected tracing result from list.
	 * 
	 * @param tracingResult
	 *            tracing result to be removed
	 */
	public synchronized void removeTracingResult(
			final TracingResultsNode tracingResult) {
		activeResultSet = null;
		TraceCollections.getFilesList().remove(tracingResult);
		TraceCollections.getTracesList().clear();
		for (final ITraceNodeObserver listener : listeners) {
			try {
				listener.removeFile();
			} catch (final Exception e) {
				ErlLogger.error(e);
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
	public IBackend getBackend(final boolean create) {
		if (tracerBackend == null && create) {
			tracerBackend = createBackend();
		}
		return tracerBackend;
	}

	/**
	 * Performs actions after loading file or trace data.
	 * 
	 * @param status
	 *            status
	 */
	private void finishLoading(final TracingStatus status) {
		for (final ITraceNodeObserver listener : listeners) {
			try {
				if (loadingFileInfo) {
					listener.finishLoadingFile(status);
				} else {
					listener.finishLoadingTraces(status);
				}
			} catch (final Exception e) {
				ErlLogger.error(e);
			}
		}
		loading = false;
		tracing = false;
	}

	private OtpErlangObject[] createProcessFlagsArray(final Set<ProcessFlag> set) {
		final OtpErlangObject[] array = new OtpErlangObject[set.size()];
		final Iterator<ProcessFlag> iterator = set.iterator();
		int i = 0;
		while (iterator.hasNext()) {
			array[i++] = iterator.next().toAtom();
		}
		return array;
	}

	public synchronized void addListener(final ITraceNodeObserver listener) {
		listeners.add(listener);
	}

	public synchronized void removeListener(final ITraceNodeObserver listener) {
		listeners.remove(listener);
	}

	public void loadTracePatterns(final TracePattern[] patterns) {
		tracePatterns.clear();
		tracePatterns.addAll(Arrays.asList(patterns));
		for (final ITraceNodeObserver listener : listeners) {
			try {
				listener.updateTracePatterns();
			} catch (final Exception e) {
				ErlLogger.error(e);
			}
		}
	}

	public Object[] getTracePatternsArray() {
		return tracePatterns.toArray();
	}

	public synchronized void addTracePattern(final TracePattern pattern) {
		tracePatterns.add(pattern);
		for (final ITraceNodeObserver listener : listeners) {
			try {
				listener.updateTracePatterns();
			} catch (final Exception e) {
				ErlLogger.error(e);
			}
		}
	}

	public synchronized void removeTracePattern(final TracePattern pattern) {
		tracePatterns.remove(pattern);
		for (final ITraceNodeObserver listener : listeners) {
			try {
				listener.updateTracePatterns();
			} catch (final Exception e) {
				ErlLogger.error(e);
			}
		}
	}

	public void loadTracedNodes(final TracedNode[] nodes) {
		tracedNodes.clear();
		tracedNodes.addAll(Arrays.asList(nodes));
	}

	public Object[] getTracedNodesArray() {
		return tracedNodes.toArray();
	}

	public synchronized void addTracedNode(final TracedNode tracedNode) {
		tracedNodes.add(tracedNode);
	}

	public synchronized void removeTracedNode(final TracedNode tracedNode) {
		tracedNodes.remove(tracedNode);
	}

	public void addProcessFlag(final ProcessFlag flag) {
		processFlags.add(flag);
	}

	public void removeProcessFlag(final ProcessFlag flag) {
		processFlags.remove(flag);
	}

	public void removeAllProcessFlag() {
		processFlags.clear();
	}

	public ProcessMode getProcessMode() {
		return processMode;
	}

	public void setProcessMode(final ProcessMode processMode) {
		this.processMode = processMode;
	}

	public void setProcesses(final TracedProcess[] processes) {
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

	/**
	 * Sets active results set (set from which traces will be loaded).
	 * 
	 * @param results
	 *            results set
	 */
	public void setActiveResultSet(final TracingResultsNode results) {
		activeResultSet = results;
	}

	public TracingResultsNode getActiveResultSet() {
		return activeResultSet;
	}

	/**
	 * Returns index of first trace to be loaded from selected result set (
	 * {@link #setActiveResultSet(TracingResultsNode)}).
	 * 
	 * @return index
	 */
	public long getStartIndex() {
		return startIndex;
	}

	private IBackend createBackend() {
		final RuntimeInfo info = RuntimeInfo.copy(BackendCore
				.getRuntimeInfoManager().getErlideRuntime(), false);
		try {
			final BackendData data = getBackendData(info);
			data.setUseStartShell(true);
			final IBackend b = BackendCore.getBackendManager()
					.createExecutionBackend(data);
			return b;
		} catch (final Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	private BackendData getBackendData(final RuntimeInfo rinfo) {
		final BackendData backendData = new BackendData(
				BackendCore.getRuntimeInfoManager(), rinfo);
		final String nodeName = Activator.getDefault().getPreferenceStore()
				.getString(PreferenceNames.NODE_NAME);
		backendData.setNodeName(nodeName);
		backendData.setConsole(false);
		backendData.setTransient(true);
		return backendData;
	}

}

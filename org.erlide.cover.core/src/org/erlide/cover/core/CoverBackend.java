package org.erlide.cover.core;

import java.util.LinkedList;
import java.util.List;

import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendData;
import org.erlide.backend.BackendException;
import org.erlide.backend.IBackend;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.cover.api.AbstractCoverRunner;
import org.erlide.cover.api.CoverException;
import org.erlide.cover.api.ICoverBackend;
import org.erlide.cover.runtime.launch.CoverLaunchData;
import org.erlide.cover.runtime.launch.CoverLaunchSettings;
import org.erlide.cover.views.model.TestTreeModel;

/**
 * Core backend for Cover-plugin
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class CoverBackend implements ICoverBackend {

	public static final String NODE_NAME = "cover_internal";

	public static CoverBackend instance;

	private IBackend backend;
	private CoverEventHandler handler;
	private CoverLaunchSettings settings;

	private final List<ICoverObserver> listeners = new LinkedList<ICoverObserver>();
	private ICoverAnnotationMarker annotationMarker;

	private final Logger log; // logger

	private EUnitEventHandler testHandler;
	private final List<IEUnitObserver> testListeners = new LinkedList<IEUnitObserver>();

	public static synchronized CoverBackend getInstance() {
		if (instance == null) {
			instance = new CoverBackend();
		}
		return instance;
	}

	private CoverBackend() {
		log = Activator.getDefault();
	}

	@Override
	public void startBackend() {
		if (backend != null && !backend.isStopped()) {
			log.info("is started");
			return;
		} else if (backend != null) {
			backend.stop();
		}

		try {
			backend = createBackend();
			handler = new CoverEventHandler(backend, this);
			handler.register();
			testHandler = new EUnitEventHandler(backend,
					TestTreeModel.getInstance(), this);
			testHandler.register();
		} catch (final BackendException e) {
			handleError("Could not create backend " + e);
		}
	}

	/**
	 * Initializes cover backend from launch configuration
	 * 
	 * @param coverData
	 * @throws CoverException
	 */
	public void initialize(/* final ErlLaunchData data, */
	final CoverLaunchData coverData) throws CoverException {

		// this.coverData = coverData;

		try {
			settings = new CoverLaunchSettings(coverData.getType(), coverData);
		} catch (final CoverException e1) {
			settings = null;
			throw e1;
		}

		startBackend();

	}

	/**
	 * Run coverage analysis
	 */
	@Override
	public synchronized void runCoverageAnalysis(
			final AbstractCoverRunner runner) {
		runner.start();
	}

	/**
	 * Get event handler that handles Erlang events
	 * 
	 * @return
	 */
	public CoverEventHandler getHandler() {
		return handler;
	}

	/**
	 * Get access to cover node
	 * 
	 * @return
	 */
	public IBackend getBackend() {
		return backend;
	}

	/**
	 * Add listener for coverage events
	 * 
	 * @param listener
	 */
	public void addListener(final ICoverObserver listener) {
		log.info("adding listener");
		listeners.add(listener);
	}

	/**
	 * Get all listeners
	 * 
	 * @return
	 */
	public List<ICoverObserver> getListeners() {
		return listeners;
	}

	/**
	 * Add listener for coverage events
	 * 
	 * @param listener
	 */
	public void addEUnitListener(final IEUnitObserver listener) {
		log.info("adding eunit listener");
		testListeners.add(listener);
	}

	/**
	 * Get all eunit listeners
	 * 
	 * @return
	 */
	public List<IEUnitObserver> getEUnitListeners() {
		return testListeners;
	}

	/**
	 * Set annotation marker for marking coverage in the editor
	 * 
	 * @param am
	 */
	public void addAnnotationMaker(final ICoverAnnotationMarker am) {
		annotationMarker = am;
	}

	/**
	 * Get annotation marker
	 * 
	 * @return
	 */
	public ICoverAnnotationMarker getAnnotationMaker() {
		return annotationMarker;
	}

	/**
	 * Handle all errors, provides graphical representation for final user.
	 * 
	 * @param msg
	 */
	public void handleError(final String msg) {
		for (final ICoverObserver obs : getListeners()) {
			obs.eventOccured(new CoverEvent(CoverStatus.ERROR, msg));
		}
	}

	/**
	 * Check coverage settings
	 * 
	 * @return
	 */
	public CoverLaunchSettings getSettings() {
		return settings;
	}

	// creates erlang backend
	private IBackend createBackend() throws BackendException {
		final RuntimeInfo info = RuntimeInfo.copy(BackendCore
				.getRuntimeInfoManager().getErlideRuntime(), false);

		if (info == null) {
			log.error(String.format("Could not find runtime %s", BackendCore
					.getRuntimeInfoManager().getErlideRuntime().getVersion()));
			handleError("Could not find runtime");
		}

		log.info("create backend");

		if (info != null) {
			try {
				final BackendData data = getBackendData(info);
				data.setNodeName(NODE_NAME);
				data.setUseStartShell(true);
				final IBackend b = BackendCore.getBackendManager()
						.createExecutionBackend(data);
				return b;
			} catch (final Exception e) {
				log.error(e);
				e.printStackTrace();
				throw new BackendException(e);
			}
		}
		throw new BackendException();
	}

	private BackendData getBackendData(final RuntimeInfo rinfo) {
		final BackendData backendData = new BackendData(
				BackendCore.getRuntimeInfoManager(), rinfo);
		backendData.setConsole(true);
		backendData.setLongName(false);
		backendData.setTransient(true);
		return backendData;
	}

}

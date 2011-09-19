package org.erlide.cover.core;

import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.erlide.core.backend.BackendCore;
import org.erlide.core.backend.BackendData;
import org.erlide.core.backend.BackendException;
import org.erlide.core.backend.BackendOptions;
import org.erlide.core.backend.ErlLaunchAttributes;
import org.erlide.core.backend.IBackend;
import org.erlide.core.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.debug.ErlangLaunchDelegate;
import org.erlide.cover.api.AbstractCoverRunner;
import org.erlide.cover.api.CoverException;
import org.erlide.cover.api.ICoverBackend;
import org.erlide.cover.runtime.launch.CoverLaunchData;
import org.erlide.cover.runtime.launch.CoverLaunchSettings;

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
    private RuntimeInfo info;
    private ILaunchConfiguration launchConfig;
    private CoverEventHandler handler;
    private CoverLaunchSettings settings;

    private final Logger log; // logger

    public static synchronized CoverBackend getInstance() {
        if (instance == null) {
            instance = new CoverBackend();
        }
        return instance;
    }

    private CoverBackend() {
        log = Activator.getDefault();
    }

    public void startBackend() {
        if (backend != null && !backend.isStopped()) {
            log.info("is started");
            return;
        } else if (backend != null) {
            backend.stop();
        }

        final RuntimeInfo rt0 = RuntimeInfo.copy(BackendCore
                .getRuntimeInfoManager().getErlideRuntime(), false);

        if (rt0 == null) {
            log.error(String.format("Could not find runtime %s", BackendCore
                    .getRuntimeInfoManager().getErlideRuntime().getVersion()));
            handleError("Could not find runtime");
        }

        log.info("create backend");

        info = buildRuntimeInfo(rt0);
        final EnumSet<BackendOptions> options = EnumSet
                .of(BackendOptions.AUTOSTART/* BackendOptions.NO_CONSOLE */);
        launchConfig = getLaunchConfiguration(info, options);

        try {
            backend = createBackend();
            handler = new CoverEventHandler(backend);
            handler.register();
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
     * Run coverage analisys
     */
    public synchronized void runCoverageAnalisys(
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
        handler.addListener(listener);
    }

    /**
     * Get all listeners
     * 
     * @return
     */
    public List<ICoverObserver> getListeners() {
        return handler.getListeners();
    }

    /**
     * Set annotation marker for marking coverage in the editor
     * 
     * @param am
     */
    public void addAnnotationMaker(final ICoverAnnotationMarker am) {
        handler.addAnnotationMaker(am);
    }

    /**
     * Get annotation marker
     * 
     * @return
     */
    public ICoverAnnotationMarker getAnnotationMaker() {
        return handler.getAnnotationMaker();
    }

    /**
     * Handle all errors, provides graphical representation for final user.
     * 
     * @param msg
     */
    public void handleError(final String msg) {
        for (final ICoverObserver obs : handler.getListeners()) {
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
        if (info != null) {
            try {
                info.setStartShell(true);

                final IBackend b = BackendCore.getBackendFactory()
                        .createBackend(
                                new BackendData(launchConfig,
                                        ILaunchManager.RUN_MODE));
                return b;
            } catch (final Exception e) {
                log.error(e);
                e.printStackTrace();
                throw new BackendException(e);
            }
        }
        throw new BackendException();
    }

    // creates runtime info
    private RuntimeInfo buildRuntimeInfo(final RuntimeInfo rt0) {
        final RuntimeInfo rt = RuntimeInfo.copy(rt0, false);
        rt.setNodeName(NODE_NAME);

        rt.setStartShell(false);

        return rt;
    }

    private ILaunchConfiguration getLaunchConfiguration(
            final RuntimeInfo myInfo, final Set<BackendOptions> options) {
        final ILaunchManager manager = DebugPlugin.getDefault()
                .getLaunchManager();
        final ILaunchConfigurationType type = manager
                .getLaunchConfigurationType(ErlangLaunchDelegate.CONFIGURATION_TYPE_INTERNAL);
        ILaunchConfigurationWorkingCopy workingCopy;

        try {
            workingCopy = type.newInstance(null,
                    "internal " + myInfo.getNodeName());
            workingCopy.setAttribute(DebugPlugin.ATTR_CONSOLE_ENCODING,
                    "ISO-8859-1");
            workingCopy.setAttribute(ErlLaunchAttributes.NODE_NAME,
                    myInfo.getNodeName());
            workingCopy.setAttribute(ErlLaunchAttributes.RUNTIME_NAME,
                    myInfo.getName());
            workingCopy.setAttribute(ErlLaunchAttributes.COOKIE,
                    myInfo.getCookie());
            workingCopy.setAttribute(ErlLaunchAttributes.CONSOLE,
                    !options.contains(BackendOptions.NO_CONSOLE));
            workingCopy.setAttribute(ErlLaunchAttributes.INTERNAL,
                    options.contains(BackendOptions.INTERNAL));
            workingCopy.setAttribute(ErlLaunchAttributes.USE_LONG_NAME, false);
            return workingCopy.doSave();
        } catch (final CoreException e) {
            e.printStackTrace();
            handleError("Error while launching backend: " + e);

            return null;
        }
    }

}

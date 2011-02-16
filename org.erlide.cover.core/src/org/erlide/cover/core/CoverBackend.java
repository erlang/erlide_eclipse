package org.erlide.cover.core;

import java.net.URL;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.erlide.backend.BackendCore;
import org.erlide.backend.runtime.RuntimeInfo;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.cover.runtime.launch.CoverLaunchData;
import org.erlide.cover.runtime.launch.LaunchType;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.ErlLaunchAttributes;
import org.erlide.jinterface.backend.ErtsProcess;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BackendManager.BackendOptions;


/**
 * Core backend for Cover-plugin
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class CoverBackend {

    public static final String NODE_NAME = "cover_internal";
    
    public static CoverBackend instance;

    private Backend backend;
    private RuntimeInfo info;
    private ILaunchConfiguration config;
    private final CoverEventHandler handler;
    private CoverLaunchData coverData;
    private CoverSettings settings;
    private String nodeName;
    private boolean coverRunning;
    
    private Logger log;      //logger
    
    static {
        // logger configuration
        URL logURL = Platform.getBundle(
                "org.erlide.cover.core").
                getEntry("/logs.conf");
        
        PropertyConfigurator.configure(logURL);
    }

    public static synchronized CoverBackend getInstance() {
        if (instance == null) {
            instance = new CoverBackend();
        }
        return instance;
    }

    private CoverBackend() {
        handler = new CoverEventHandler();
        coverRunning = false;
        log = Logger.getLogger(this.getClass());
    }

    public void initialize(/* final ErlLaunchData data, */
            final CoverLaunchData coverData) {

        this.coverData = coverData;

        settings = new CoverSettings(coverData.getType(), coverData);

        if (backend != null && !backend.isStopped()) {
        	log.debug("is started");
            return;
        } else if (backend != null) {
            backend.stop();
        }

        RuntimeInfo rt0  = RuntimeInfo.copy(BackendCore
                .getRuntimeInfoManager().getErlideRuntime(), false);
        

        if (rt0 == null) {
            log.error(String.format("Could not find runtime %s", BackendCore
                    .getRuntimeInfoManager().getErlideRuntime().getVersion()));
            handleError("Could not find runtime");
        }

        log.debug("create backend");
        
        info = buildRuntimeInfo(rt0);
        final EnumSet<BackendOptions> options = EnumSet
                .of(BackendOptions.AUTOSTART/* BackendOptions.NO_CONSOLE */);
        config = getLaunchConfiguration(info, options);

        try {
            backend = createBackend();
            backend.restart();
            backend.getEventDaemon().addHandler(handler);
        } catch (final BackendException e) {
            handleError("Could not create backend " + e);
        }

    }

    public void attachBackend(final Backend b, final LaunchType type) {

        // no set config
        // TODO: implement
    }

    public void attachToNode(final String nodeName) {
        // TODO: check how you can attach to nodes
        // see how to obtain backend
    }

    public synchronized void start() {

        if (!coverRunning) {
            coverRunning = true;
            new CoverRunner(this).start();
        }

    }

    public synchronized void coverageFinished() {
        coverRunning = false;
    }

    public CoverEventHandler getHandler() {
        return handler;
    }

    public Backend getBackend() {
        return backend;
    }

    public void addListener(final ICoverObserver listener) {
        handler.addListener(listener);
    }

    public List<ICoverObserver> getListeners() {
        return handler.getListeners();
    }

    public void addAnnotationMaker(final ICoverAnnotationMarker am) {
        handler.addAnnotationMaker(am);
    }

    public ICoverAnnotationMarker getAnnotationMaker() {
        return handler.getAnnotationMaker();
    }

    public void handleError(final String msg) {
        for (final ICoverObserver obs : handler.getListeners()) {
            obs.eventOccured(new CoverEvent(CoverStatus.ERROR, msg));
        }
    }

    public CoverSettings getSettings() {
        return settings;
    }

    // input from external plugins
    public void setPathsToCover(final List<String> filePaths) {
        // TODO: ~custom coverage
    }

    private Backend createBackend() throws BackendException {
        if (info != null) {
            try {
                info.setStartShell(true);

                config.launch(ILaunchManager.RUN_MODE,
                        new NullProgressMonitor(), false, false);

                return BackendManager.getDefault().getByName(nodeName);
            } catch (final Exception e) {
                log.error(e);
                e.printStackTrace();
                throw new BackendException(e);
            }
        }
        throw new BackendException();
    }

    private RuntimeInfo buildRuntimeInfo(final RuntimeInfo rt0) {
        final RuntimeInfo rt = RuntimeInfo.copy(rt0, false);
        rt.setNodeName(NODE_NAME);

        rt.setStartShell(false);

        return rt;
    }

    private ILaunchConfiguration getLaunchConfiguration(final RuntimeInfo info,
            final Set<BackendOptions> options) {
        final ILaunchManager manager = DebugPlugin.getDefault()
                .getLaunchManager();
        final ILaunchConfigurationType type = manager
                .getLaunchConfigurationType(ErtsProcess.CONFIGURATION_TYPE_INTERNAL);
        ILaunchConfigurationWorkingCopy workingCopy;

        nodeName = info.getNodeName();
        try {
            workingCopy = type.newInstance(null,
                    "internal " + info.getNodeName());
            workingCopy.setAttribute(DebugPlugin.ATTR_CONSOLE_ENCODING,
                    "ISO-8859-1");
            workingCopy.setAttribute(ErlLaunchAttributes.NODE_NAME,
                    info.getNodeName());
            workingCopy.setAttribute(ErlLaunchAttributes.RUNTIME_NAME,
                    info.getName());
            workingCopy.setAttribute(ErlLaunchAttributes.COOKIE,
                    info.getCookie());
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

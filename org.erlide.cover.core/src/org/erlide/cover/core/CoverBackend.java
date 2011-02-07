package org.erlide.cover.core;

import java.io.File;
import java.net.URL;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.cover.runtime.launch.CoverLaunchData;
import org.erlide.cover.runtime.launch.LaunchType;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BackendManager.BackendOptions;
import org.erlide.runtime.backend.ErtsProcess;
import org.erlide.runtime.launch.ErlLaunchAttributes;
import org.erlide.runtime.launch.ErlLaunchData;

/**
 * Core backend for Cover-plugin
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class CoverBackend {

    public static CoverBackend instance;

    private Backend backend;
    private RuntimeInfo info;
    private ILaunchConfiguration config;
    private final CoverEventHandler handler;
    private CoverLaunchData coverData;
    private CoverSettings settings;
    private String nodeName;
    private boolean coverRunning;
    
    private Logger log;  //logger
    
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

    public void initialize(final ErlLaunchData data,
            final CoverLaunchData coverData) {

        this.coverData = coverData;

        settings = new CoverSettings(coverData.getType(), coverData);

        if (backend != null && !backend.isStopped()
                && info.getNodeName().equals(data.nodeName)
                && info.getName().equals(data.runtime)) {
            return;
        } else if (backend != null) {
            backend.stop();
        }

        final RuntimeInfo rt0 = ErlangCore.getRuntimeInfoManager().getRuntime(
                data.runtime);

        if (rt0 == null) {
            ErlLogger.error("Could not find runtime %s", data.runtime);
            handleError("Could not find runtime");
        }

        log.debug("create backend");
        //ErlLogger.debug("Backend created...");
        //System.out.println("Create backend");

        info = buildRuntimeInfo(data, rt0);
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
                ErlLogger.error(e);
                e.printStackTrace();
                throw new BackendException(e);
            }
        }
        throw new BackendException();
    }

    private RuntimeInfo buildRuntimeInfo(final ErlLaunchData data,
            final RuntimeInfo rt0) {
        final RuntimeInfo rt = RuntimeInfo.copy(rt0, false);
        rt.setNodeName(data.nodeName);
        rt.setCookie(data.cookie);

        rt.setStartShell(true);
        final File d = new File(data.workingDir);
        if (d.isAbsolute()) {
            rt.setWorkingDir(data.workingDir);
        } else {
            final String wspace = ResourcesPlugin.getWorkspace().getRoot()
                    .getLocation().toPortableString();
            rt.setWorkingDir(wspace + "/" + data.workingDir);
        }
        rt.setArgs(data.xtraArgs);
        rt.useLongName(data.longName);
        rt.setHasConsole(data.console);
        rt.setLoadAllNodes(data.loadAllNodes);

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

package org.erlide.cover.core;

import java.io.File;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.cover.constants.Constants;
import org.erlide.cover.runtime.launch.CoverLaunchData;
import org.erlide.cover.runtime.launch.LaunchType;
import org.erlide.cover.views.model.StatsTreeModel;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BackendManager.BackendOptions;
import org.erlide.runtime.backend.ErtsProcess;
import org.erlide.runtime.launch.ErlLaunchAttributes;
import org.erlide.runtime.launch.ErlLaunchData;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Core backend for Cover-plugin
 * 
 * @author Aleksandra Lipiec
 * 
 */
public class CoverBackend {

    public static CoverBackend instance;
    
    private Backend backend;
    private RuntimeInfo info;
    private ILaunchConfiguration config;
    private CoverEventHandler handler;
    private CoverLaunchData coverData;
    private CoverSettings settings;
    private String nodeName;
    private boolean coverRunning;

    public static CoverBackend getInstance() {
        if (instance == null)
            instance = new CoverBackend();
        return instance;
    }

    private CoverBackend() {
        handler = new CoverEventHandler();
        coverRunning = false;
    }

    public void initialize(ErlLaunchData data, CoverLaunchData coverData)
            throws RuntimeException, BackendException {

        if (backend != null)
            backend.stop();

        final RuntimeInfo rt0 = ErlangCore.getRuntimeInfoManager().getRuntime(
                data.runtime);

        if (rt0 == null) {
            ErlLogger.error("Could not find runtime %s", data.runtime);
            throw new RuntimeException();
        }

        this.coverData = coverData;

        settings = new CoverSettings(coverData.getType(), coverData);

        ErlLogger.debug("Backend created...");
        System.out.println("Create backend");

        this.info = buildRuntimeInfo(data, rt0);
        EnumSet<BackendOptions> options = EnumSet
                .of(BackendOptions.AUTOSTART/* BackendOptions.NO_CONSOLE */);
        this.config = getLaunchConfiguration(info, options);

        this.backend = createBackend();

        backend.getEventDaemon().addHandler(handler);

    }

    public void attachBackend(Backend b, LaunchType type) {
        backend.stop();
        backend = b;

        settings = new CoverSettings(type, null);
        this.info = b.getInfo();
        // no set config

    }

    public void attachToNode(String nodeName) {
        // TODO: check how you can attach to nodes
        // see how to obtain backend
    }

    public synchronized void start() {
        
        if(!coverRunning) {
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

    public void addListener(ICoverObserver listener) {
        handler.addListener(listener);
    }

    public List<ICoverObserver> getListeners() {
        return handler.getListeners();
    }
    
    public CoverSettings getSettings() {
        return settings;
    }

    // input from external plugins
    public void setPathsToCover(List<String> filePaths) {
        // TODO: ~custom coverage
    }

    private Backend createBackend() throws BackendException {
        if (info != null) {
            try {
                info.setStartShell(true);

                ErlLogger.debug("launching....");
                System.out.println("Creating Backend");

                config.launch(ILaunchManager.RUN_MODE,
                        new NullProgressMonitor(), false, false);

                System.out.println("after launching");

                return BackendManager.getDefault().getByName(nodeName);
            } catch (Exception e) {
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
        rt.hasConsole(data.console);
        rt.setLoadAllNodes(data.loadAllNodes);
        

        System.out.println("runtimeInfo build");

        return rt;
    }

    private ILaunchConfiguration getLaunchConfiguration(RuntimeInfo info,
            Set<BackendOptions> options) {
        ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
        ILaunchConfigurationType type = manager
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
        } catch (CoreException e) {
            e.printStackTrace();
            return null;
        }
    }

}

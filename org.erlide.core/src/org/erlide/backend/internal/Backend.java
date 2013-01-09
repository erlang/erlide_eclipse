/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend.internal;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.RegistryFactory;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.backend.BackendData;
import org.erlide.backend.BackendException;
import org.erlide.backend.IBackend;
import org.erlide.backend.IBackendManager;
import org.erlide.backend.ICodeBundle;
import org.erlide.backend.ICodeManager;
import org.erlide.backend.console.BackendShellManager;
import org.erlide.backend.console.IBackendShell;
import org.erlide.backend.console.IoRequest.IoRequestKind;
import org.erlide.backend.events.ErlangEventPublisher;
import org.erlide.backend.events.ErlangLogEventHandler;
import org.erlide.backend.events.LogEventHandler;
import org.erlide.core.ErlangCore;
import org.erlide.core.model.ErlModelException;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.util.ErlideUtil;
import org.erlide.launch.debug.ErlideDebug;
import org.erlide.launch.debug.model.ErlangDebugNode;
import org.erlide.launch.debug.model.ErlangDebugTarget;
import org.erlide.runtime.BeamLoader;
import org.erlide.runtime.ErlDebugFlags;
import org.erlide.runtime.IErlRuntime;
import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.InitialCall;
import org.erlide.runtime.RuntimeData;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.utils.Asserts;
import org.erlide.utils.ErlLogger;
import org.erlide.utils.SystemConfiguration;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;

public abstract class Backend implements IStreamListener, IBackend {

    private final IErlRuntime runtime;
    private String erlangVersion;
    private ErlangEventPublisher eventDaemon;
    private BackendShellManager shellManager;
    private final ICodeManager codeManager;
    private final BackendData data;
    private ErlangDebugTarget debugTarget;
    private final IBackendManager backendManager;

    public Backend(final BackendData data, final IErlRuntime runtime,
            final IBackendManager backendManager) throws BackendException {
        Asserts.isNotNull(data.getRuntimeInfo());
        this.runtime = runtime;
        this.data = data;
        this.backendManager = backendManager;
        codeManager = new CodeManager(this, getRuntimeInfo(), backendManager);
    }

    @Override
    public void dispose() {
        ErlLogger.debug("disposing backend " + getName());
        if (shellManager != null) {
            shellManager.dispose();
        }

        if (eventDaemon != null) {
            eventDaemon.stop();
        }
        runtime.stop();
    }

    @Override
    public String getErlangVersion() {
        if (erlangVersion == null) {
            try {
                erlangVersion = getScriptId();
            } catch (final Exception e) {
            }
        }
        return erlangVersion;
    }

    @Override
    public RuntimeInfo getRuntimeInfo() {
        return data.getRuntimeInfo();
    }

    @Override
    public String getName() {
        if (runtime == null) {
            return "<not_connected>";
        }
        return runtime.getNodeName();
    }

    private String getScriptId() throws RpcException {
        OtpErlangObject r;
        r = getRpcSite().call("init", "script_id", "");
        if (r instanceof OtpErlangTuple) {
            final OtpErlangObject rr = ((OtpErlangTuple) r).elementAt(1);
            if (rr instanceof OtpErlangString) {
                return ((OtpErlangString) rr).stringValue();
            }
        }
        return "";
    }

    private boolean startErlangApps(final OtpErlangPid jRex, final boolean watch) {
        try {
            getRpcSite().call(
                    "erlide_kernel_common",
                    "init",
                    "poii",
                    jRex,
                    watch,
                    SystemConfiguration.getInstance()
                            .getWarnProcessSizeLimitMB(),
                    SystemConfiguration.getInstance()
                            .getKillProcessSizeLimitMB());
            // TODO should use extension point!
            getRpcSite().call("erlide_kernel_builder", "init", "");
            getRpcSite().call("erlide_kernel_ide", "init", "");

            // TODO start tracing when configured to do so!
            // getRpcSite().call("erlide_tracer", "start", "");
            return true;
        } catch (final Exception e) {
            ErlLogger.error(e);
            return false;
        }
    }

    @Override
    public boolean isStopped() {
        return runtime.isStopped();
    }

    @Override
    public void start() {
        runtime.start();
    }

    @Override
    public void stop() {
        if (data.isDebug()) {
            unloadDebuggerCode();
        }
        runtime.stop();
    }

    @Override
    public OtpMbox createMbox() {
        return runtime.createMbox();
    }

    @Override
    public OtpMbox createMbox(final String name) {
        return runtime.createMbox(name);
    }

    public void removePath(final String path) {
        codeManager.removePath(path);
    }

    public void addPath(final boolean usePathZ, final String path) {
        codeManager.addPath(usePathZ, path);
    }

    public synchronized void initErlang(final boolean watch) {
        ErlLogger.debug("initialize %s: %s", getName(), watch);
        startErlangApps(getEventPid(), watch);

        eventDaemon = new ErlangEventPublisher(this);
        eventDaemon.start();
        new LogEventHandler(this).register();
        new ErlangLogEventHandler(this).register();

        backendManager.addBackendListener(eventDaemon.getBackendListener());
    }

    @Override
    public void registerCodeBundle(final ICodeBundle bundle) {
        codeManager.register(bundle);
    }

    @Override
    public void unregisterCodeBundle(final Bundle b) {
        codeManager.unregister(b);
    }

    @Override
    public void streamAppended(final String text, final IStreamMonitor monitor) {
        final IStreamsProxy proxy = getStreamsProxy();
        if (monitor == proxy.getOutputStreamMonitor()) {
            // System.out.println(getName() + " OUT " + text);
        } else if (monitor == proxy.getErrorStreamMonitor()) {
            // System.out.println(getName() + " ERR " + text);
        } else {
            // System.out.println("???" + text);
        }
    }

    @Override
    public ILaunch getLaunch() {
        return data.getLaunch();
    }

    public void assignStreamProxyListeners() {
        final IStreamsProxy proxy = getStreamsProxy();
        if (proxy != null) {
            final IStreamMonitor errorStreamMonitor = proxy
                    .getErrorStreamMonitor();
            errorStreamMonitor.addListener(this);
            final IStreamMonitor outputStreamMonitor = proxy
                    .getOutputStreamMonitor();
            outputStreamMonitor.addListener(this);
        }
    }

    @Override
    public IBackendShell getShell(final String id) {
        final IBackendShell shell = shellManager.openShell(id);
        final IStreamsProxy proxy = getStreamsProxy();
        if (proxy != null) {
            final IStreamMonitor errorStreamMonitor = proxy
                    .getErrorStreamMonitor();
            errorStreamMonitor.addListener(new IStreamListener() {
                @Override
                public void streamAppended(final String text,
                        final IStreamMonitor monitor) {
                    shell.add(text, IoRequestKind.STDERR);
                }
            });
            final IStreamMonitor outputStreamMonitor = proxy
                    .getOutputStreamMonitor();
            outputStreamMonitor.addListener(new IStreamListener() {
                @Override
                public void streamAppended(final String text,
                        final IStreamMonitor monitor) {
                    shell.add(text, IoRequestKind.STDOUT);
                }
            });
        }
        return shell;
    }

    @Override
    public boolean isDistributed() {
        return !Strings.isNullOrEmpty(getData().getNodeName());
    }

    @Override
    public void input(final String s) throws IOException {
        if (!isStopped()) {
            final IStreamsProxy proxy = getStreamsProxy();
            if (proxy != null) {
                proxy.write(s);
            } else {
                ErlLogger
                        .warn("Could not load module on backend %s, stream proxy is null",
                                getRuntimeInfo());
            }
        }
    }

    @Override
    public void addProjectPath(final IErlProject eproject) {
        final IProject project = eproject.getWorkspaceProject();
        final String outDir = project.getLocation()
                .append(eproject.getOutputLocation()).toOSString();
        if (outDir.length() > 0) {
            ErlLogger.debug("backend %s: add path %s", getName(), outDir);
            if (isDistributed()) {
                final boolean accessible = ErlideUtil.isAccessibleDir(
                        getRpcSite(), outDir);
                if (accessible) {
                    addPath(false/* prefs.getUsePathZ() */, outDir);
                } else {
                    loadBeamsFromDir(outDir);
                }
            }
        }
    }

    @Override
    public void removeProjectPath(final IErlProject eproject) {
        if (eproject == null) {
            // can happen if project was removed
            return;
        }
        final IProject project = eproject.getWorkspaceProject();
        final String outDir = project.getLocation()
                .append(eproject.getOutputLocation()).toOSString();
        if (outDir.length() > 0) {
            ErlLogger.debug("backend %s: remove path %s", getName(), outDir);
            if (isDistributed()) {
                final boolean accessible = ErlideUtil.isAccessibleDir(
                        getRpcSite(), outDir);
                if (accessible) {
                    removePath(outDir);
                } else {
                    // FIXME unloadBeamsFromDir(outDir);
                }
            }
        }
    }

    private void loadBeamsFromDir(final String outDir) {
        final File dir = new File(outDir);
        if (dir.isDirectory()) {
            for (final File f : dir.listFiles()) {
                final Path path = new Path(f.getPath());
                if (path.getFileExtension() != null
                        && "beam".compareTo(path.getFileExtension()) == 0) {
                    final String m = path.removeFileExtension().lastSegment();
                    try {
                        boolean ok = false;
                        final OtpErlangBinary bin = BeamUtil.getBeamBinary(m,
                                path);
                        if (bin != null) {
                            ok = BeamLoader.loadBeam(getRpcSite(), m, bin);
                        }
                        if (!ok) {
                            ErlLogger.error("Could not load %s", m);
                        }
                    } catch (final Exception ex) {
                        ErlLogger.warn(ex);
                    }
                }
            }
        }
    }

    @Override
    public boolean isManaged() {
        return data.isManaged();
    }

    @Override
    public boolean shouldLoadOnAllNodes() {
        return data.shouldLoadOnAllNodes();
    }

    @Override
    public IStreamsProxy getStreamsProxy() {
        return null;
    }

    protected void postLaunch() throws DebugException {
        final Collection<IProject> projects = Lists.newArrayList(data
                .getProjects());
        registerProjectsWithExecutionBackend(projects);
        if (!isDistributed()) {
            return;
        }
        if (data.isDebug()) {
            // add debug debugTarget
            debugTarget = new ErlangDebugTarget(getLaunch(), this, projects,
                    data.getDebugFlags());
            // debugTarget.getWaiter().doWait();
            getLaunch().addDebugTarget(debugTarget);
            // interpret everything we can
            final boolean distributed = (data.getDebugFlags()
                    .contains(ErlDebugFlags.DISTRIBUTED_DEBUG));
            if (distributed) {
                distributeDebuggerCode();
                addNodesAsDebugTargets(getLaunch(), debugTarget);
            }
            interpretModules(data, distributed);
            registerStartupFunctionStarter(data);
            debugTarget.sendStarted();
        } else {
            final InitialCall init_call = data.getInitialCall();
            if (init_call != null) {
                runInitial(init_call.getModule(), init_call.getName(),
                        init_call.getParameters());
            }
        }
    }

    private void registerProjectsWithExecutionBackend(
            final Collection<IProject> projects) {
        for (final IProject project : projects) {
            backendManager.addExecutionBackend(project, this);
        }
    }

    private void registerStartupFunctionStarter(final BackendData myData) {
        DebugPlugin.getDefault().addDebugEventListener(
                new IDebugEventSetListener() {
                    @Override
                    public void handleDebugEvents(final DebugEvent[] events) {
                        final InitialCall init_call = myData.getInitialCall();
                        if (init_call != null) {
                            runInitial(init_call.getModule(),
                                    init_call.getName(),
                                    init_call.getParameters());
                        }
                        DebugPlugin.getDefault().removeDebugEventListener(this);
                    }
                });
    }

    void runInitial(final String module, final String function,
            final String args) {
        try {
            if (module.length() > 0 && function.length() > 0) {
                ErlLogger.debug("calling startup function %s:%s", module,
                        function);
                if (args.length() > 0) {
                    getRpcSite().cast(module, function, "s", args);
                } else {
                    getRpcSite().cast(module, function, "");
                }
            }
        } catch (final Exception e) {
            ErlLogger.debug("Could not run initial call %s:%s(\"%s\")", module,
                    function, args);
            ErlLogger.warn(e);
        }
    }

    private void interpretModules(final BackendData myData,
            final boolean distributed) {
        for (final String pm : data.getInterpretedModules()) {
            final String[] pms = pm.split(":");
            final IProject project = ResourcesPlugin.getWorkspace().getRoot()
                    .getProject(pms[0]);
            interpret(project, pms[1], distributed, true);
        }
    }

    private void addNodesAsDebugTargets(final ILaunch aLaunch,
            final ErlangDebugTarget target) {
        final OtpErlangList nodes = ErlideDebug.nodes(getRpcSite());
        if (nodes != null) {
            for (int i = 1, n = nodes.arity(); i < n; ++i) {
                final OtpErlangAtom o = (OtpErlangAtom) nodes.elementAt(i);
                final OtpErlangAtom a = o;
                final ErlangDebugNode edn = new ErlangDebugNode(target,
                        a.atomValue());
                aLaunch.addDebugTarget(edn);
            }
        }
    }

    private void distributeDebuggerCode() {
        final List<String> debuggerModules = getDebuggerModules();

        final List<OtpErlangTuple> modules = new ArrayList<OtpErlangTuple>(
                debuggerModules.size());
        for (final String module : debuggerModules) {
            final OtpErlangBinary b = getDebuggerBeam(module);
            if (b != null) {
                final OtpErlangString filename = new OtpErlangString(module
                        + ".erl");
                final OtpErlangTuple t = OtpErlang.mkTuple(new OtpErlangAtom(
                        module), filename, b);
                modules.add(t);
            } else {
                ErlLogger.warn("Could not find debugger module %s", module);
            }
        }
        ErlideDebug.distributeDebuggerCode(getRpcSite(), modules);
    }

    private void unloadDebuggerCode() {
        final List<String> debuggerModules = getDebuggerModules();
        ErlideDebug.unloadDebuggerCode(getRpcSite(), debuggerModules);
    }

    private List<String> getDebuggerModules() {
        final Bundle debugger = Platform
                .getBundle("org.erlide.kernel.debugger");
        final List<String> debuggerModules = Lists.newArrayList();
        @SuppressWarnings("rawtypes")
        final Enumeration beams = debugger
                .findEntries("/ebin", "*.beam", false);
        while (beams.hasMoreElements()) {
            final URL beam = (URL) beams.nextElement();
            debuggerModules.add(new Path(beam.getPath()).removeFileExtension()
                    .lastSegment());
        }
        return debuggerModules;
    }

    /**
     * Get a named beam-file as a binary from the core plug-in bundle
     * 
     * @param module
     *            module name, without extension
     * @param backend
     *            the execution backend
     * @return
     */
    private OtpErlangBinary getDebuggerBeam(final String module) {
        final String beamname = module + ".beam";
        final Bundle bundle = Platform.getBundle("org.erlide.kernel.debugger");

        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] els = reg.getConfigurationElementsFor(
                ErlangCore.PLUGIN_ID, "codepath");

        // TODO: this code assumes that the debugged debugTarget and the
        // erlide-plugin uses the same Erlang version, how can we escape this?

        final String ver = getErlangVersion();
        for (final IConfigurationElement el : els) {
            final IContributor c = el.getContributor();
            final String name = c.getName();
            if (name.equals(bundle.getSymbolicName())) {
                final String dir_path = el.getAttribute("path");
                Enumeration<?> e = bundle.getEntryPaths(dir_path + "/" + ver);
                if (e == null || !e.hasMoreElements()) {
                    e = bundle.getEntryPaths(dir_path);
                }
                if (e == null) {
                    ErlLogger.debug("* !!! error loading plugin "
                            + bundle.getSymbolicName());
                    return null;
                }
                while (e.hasMoreElements()) {
                    final String s = (String) e.nextElement();
                    final Path path = new Path(s);
                    if (path.lastSegment().equals(beamname)) {
                        return getBeamFromBundlePath(bundle, s, path);
                    }
                }
            }
        }
        return null;
    }

    private OtpErlangBinary getBeamFromBundlePath(final Bundle bundle,
            final String s, final Path path) {
        final String m = path.removeFileExtension().lastSegment();
        try {
            return BeamUtil.getBeamBinary(m, bundle.getEntry(s));
        } catch (final Exception ex) {
            ErlLogger.warn(ex);
            return null;
        }
    }

    @Override
    public boolean hasConsole() {
        return getData().hasConsole();
    }

    @Override
    public BackendData getData() {
        return data;
    }

    @Override
    public void initialize() {
        shellManager = new BackendShellManager(this);
        if (isDistributed()) {
            connect();
            for (final ICodeBundle bb : backendManager.getCodeBundles()
                    .values()) {
                registerCodeBundle(bb);
            }
            initErlang(data.isManaged());

            try {
                postLaunch();
            } catch (final DebugException e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    public void installDeferredBreakpoints() {
        debugTarget.installDeferredBreakpoints();
    }

    @Override
    public void interpret(final IProject project, final String moduleName,
            final boolean distributed, final boolean interpret) {
        try {
            final IFile beam = data.getBeamLocator().findModuleBeam(project,
                    moduleName);
            if (beam != null) {
                if (beam.exists()) {
                    final String de = interpret ? "" : "de";
                    ErlLogger.debug(de + "interpret " + beam.getLocation());
                    boolean b = ErlideDebug.interpret(getRpcSite(), beam
                            .getLocation().toString(), distributed, interpret);
                    b = !b;
                } else {
                    ErlLogger.debug("IGNORED MISSING interpret "
                            + (project == null ? "null" : project.getName())
                            + ":" + moduleName);
                }
            } else {
                ErlLogger.debug("IGNORED NULL interpret "
                        + (project == null ? "null" : project.getName()) + ":"
                        + moduleName);
            }
        } catch (final ErlModelException e) {
            ErlLogger.warn(e);
        }
    }

    // /////

    @Override
    public boolean isAvailable() {
        return runtime.isAvailable();
    }

    @Override
    public String getNodeName() {
        return runtime.getNodeName();
    }

    @Override
    public OtpErlangPid getEventPid() {
        return runtime.getEventPid();
    }

    @Override
    public void connect() {
        runtime.connect();
    }

    @Override
    public OtpMbox getEventMbox() {
        return runtime.getEventMbox();
    }

    @Override
    public IRpcSite getRpcSite() {
        return runtime.getRpcSite();
    }

    @Override
    public RuntimeData getRuntimeData() {
        return data;
    }
}

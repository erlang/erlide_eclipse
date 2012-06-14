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
import java.net.Socket;
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
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendData;
import org.erlide.backend.BackendException;
import org.erlide.backend.BackendHelper;
import org.erlide.backend.IBackend;
import org.erlide.backend.IBackendManager;
import org.erlide.backend.ICodeBundle;
import org.erlide.backend.ICodeManager;
import org.erlide.backend.IErlRuntime;
import org.erlide.backend.InitialCall;
import org.erlide.backend.console.BackendShellManager;
import org.erlide.backend.console.IBackendShell;
import org.erlide.backend.console.IoRequest.IoRequestKind;
import org.erlide.backend.events.ErlangEventPublisher;
import org.erlide.backend.events.ErlangLogEventHandler;
import org.erlide.backend.events.LogEventHandler;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.ErlangCore;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.util.ErlideUtil;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.IRpcCallback;
import org.erlide.jinterface.rpc.IRpcFuture;
import org.erlide.jinterface.rpc.IRpcResultCallback;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcHelper;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.launch.debug.ErlDebugConstants;
import org.erlide.launch.debug.ErlideDebug;
import org.erlide.launch.debug.model.ErlangDebugNode;
import org.erlide.launch.debug.model.ErlangDebugTarget;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.SignatureException;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;

public abstract class Backend implements IStreamListener, IBackend {

    private static final String COULD_NOT_CONNECT_TO_BACKEND = "Could not connect to backend! Please check runtime settings.";
    private static final int EPMD_PORT = 4369;
    public static int DEFAULT_TIMEOUT;
    {
        setDefaultTimeout();
    }

    private final RuntimeInfo info;
    private final IErlRuntime runtime;
    private String erlangVersion;
    private OtpMbox eventBox;
    private boolean stopped = false;
    private ErlangEventPublisher eventDaemon;
    private BackendShellManager shellManager;
    private final ICodeManager codeManager;
    private final BackendData data;
    private ErlangDebugTarget debugTarget;

    public Backend(final BackendData data, final IErlRuntime runtime)
            throws BackendException {
        info = data.getRuntimeInfo();
        if (info == null) {
            throw new BackendException(
                    "Can't create backend without runtime information");
        }
        this.runtime = runtime;
        this.data = data;
        codeManager = new CodeManager(this, getErlangVersion(),
                getRuntimeInfo());
    }

    @Override
    public RpcResult call_noexception(final String m, final String f,
            final String signature, final Object... a) {
        return call_noexception(DEFAULT_TIMEOUT, m, f, signature, a);
    }

    @Override
    public RpcResult call_noexception(final int timeout, final String m,
            final String f, final String signature, final Object... args) {
        try {
            final OtpErlangObject result = runtime.makeCall(timeout, m, f,
                    signature, args);
            return new RpcResult(result);
        } catch (final RpcException e) {
            return RpcResult.error(e.getMessage());
        } catch (final SignatureException e) {
            return RpcResult.error(e.getMessage());
        }
    }

    @Override
    public IRpcFuture async_call(final String m, final String f,
            final String signature, final Object... args) throws RpcException {
        try {
            return runtime.makeAsyncCall(m, f, signature, args);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public void async_call_cb(final IRpcCallback cb, final String m,
            final String f, final String signature, final Object... args)
            throws RpcException {
        try {
            runtime.makeAsyncCbCall(cb, DEFAULT_TIMEOUT, m, f, signature, args);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public void async_call_result(final IRpcResultCallback cb, final String m,
            final String f, final String signature, final Object... args)
            throws RpcException {
        try {
            runtime.makeAsyncResultCall(cb, m, f, signature, args);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public void cast(final String m, final String f, final String signature,
            final Object... args) throws RpcException {
        try {
            runtime.makeCast(m, f, signature, args);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public OtpErlangObject call(final String m, final String f,
            final String signature, final Object... a) throws RpcException {
        return call(DEFAULT_TIMEOUT, m, f, signature, a);
    }

    @Override
    public OtpErlangObject call(final int timeout, final String m,
            final String f, final String signature, final Object... a)
            throws RpcException {
        return call(timeout, new OtpErlangAtom("user"), m, f, signature, a);
    }

    @Override
    public OtpErlangObject call(final int timeout,
            final OtpErlangObject gleader, final String m, final String f,
            final String signature, final Object... a) throws RpcException {
        try {
            return runtime.makeCall(timeout, gleader, m, f, signature, a);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public void send(final OtpErlangPid pid, final Object msg) {
        try {
            runtime.send(pid, msg);
        } catch (final SignatureException e) {
            ErlLogger.warn(e);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
    }

    @Override
    public void send(final String name, final Object msg) {
        try {
            runtime.send(getFullNodeName(), name, msg);
        } catch (final SignatureException e) {
            ErlLogger.warn(e);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
    }

    @Override
    public OtpErlangObject receiveEvent(final long timeout)
            throws OtpErlangExit, OtpErlangDecodeException {
        if (eventBox == null) {
            return null;
        }
        return eventBox.receive(timeout);
    }

    @Override
    public void connect() {
        final String label = getName();
        ErlLogger.debug(label + ": waiting connection to peer...");
        try {
            wait_for_epmd();
            eventBox = runtime.createMbox("rex");

            if (waitForCodeServer()) {
                ErlLogger.debug("connected!");
            } else {
                ErlLogger.error(COULD_NOT_CONNECT_TO_BACKEND);
            }

        } catch (final BackendException e) {
            ErlLogger.error(e);
            ErlLogger.error(COULD_NOT_CONNECT_TO_BACKEND);
        } catch (final Exception e) {
            ErlLogger.error(e);
            ErlLogger.error(COULD_NOT_CONNECT_TO_BACKEND);
        }
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

    private OtpMbox getEventBox() {
        return eventBox;
    }

    @Override
    public OtpErlangPid getEventPid() {
        final OtpMbox theEventBox = getEventBox();
        if (theEventBox == null) {
            return null;
        }
        return theEventBox.self();
    }

    @Override
    public RuntimeInfo getRuntimeInfo() {
        return info;
    }

    @Override
    public String getName() {
        if (runtime == null) {
            return "<not_connected>";
        }
        return runtime.getNodeName();
    }

    @Override
    public String getFullNodeName() {
        return runtime.getNodeName();
    }

    private String getScriptId() throws RpcException {
        OtpErlangObject r;
        r = call("init", "script_id", "");
        if (r instanceof OtpErlangTuple) {
            final OtpErlangObject rr = ((OtpErlangTuple) r).elementAt(1);
            if (rr instanceof OtpErlangString) {
                return ((OtpErlangString) rr).stringValue();
            }
        }
        return "";
    }

    private boolean init(final OtpErlangPid jRex, final boolean monitor,
            final boolean watch) {
        try {
            call("erlide_kernel_common", "init", "poo", jRex, monitor, watch);
            // TODO should use extension point!
            call("erlide_kernel_builder", "init", "");
            call("erlide_kernel_ide", "init", "");
            return true;
        } catch (final Exception e) {
            ErlLogger.error(e);
            return false;
        }
    }

    @Override
    public boolean isStopped() {
        return stopped || !runtime.isAvailable();
    }

    @Override
    public void stop() {
        stopped = true;
    }

    private void wait_for_epmd() throws BackendException {
        wait_for_epmd("localhost");
    }

    private void wait_for_epmd(final String host) throws BackendException {
        // If anyone has a better solution for waiting for epmd to be up, please
        // let me know
        int tries = 50;
        boolean ok = false;
        do {
            Socket s;
            try {
                s = new Socket(host, EPMD_PORT);
                s.close();
                ok = true;
            } catch (final IOException e) {
            }
            try {
                Thread.sleep(100);
                // ErlLogger.debug("sleep............");
            } catch (final InterruptedException e1) {
            }
            tries--;
        } while (!ok && tries > 0);
        if (!ok) {
            final String msg = "Couldn't contact epmd - erlang backend is probably not working\n"
                    + "  Possibly your host's entry in /etc/hosts is wrong.";
            ErlLogger.error(msg);
            throw new BackendException(msg);
        }
    }

    private boolean waitForCodeServer() {
        try {
            OtpErlangObject r;
            int i = 10;
            do {
                r = call("erlang", "whereis", "a", "code_server");
                try {
                    Thread.sleep(200);
                } catch (final InterruptedException e) {
                }
                i--;
            } while (!(r instanceof OtpErlangPid) && i > 0);
            if (!(r instanceof OtpErlangPid)) {
                ErlLogger.error("code server did not start in time for %s",
                        getRuntimeInfo().getName());
                return false;
            }
            ErlLogger.debug("code server started");
            return true;
        } catch (final Exception e) {
            ErlLogger.error("error starting code server for %s: %s",
                    getRuntimeInfo().getName(), e.getMessage());
            return false;
        }
    }

    @Override
    public OtpMbox createMbox() {
        return runtime.createMbox();
    }

    @Override
    public OtpMbox createMbox(final String name) {
        return runtime.createMbox(name);
    }

    private static void setDefaultTimeout() {
        final String t = System.getProperty("erlide.rpc.timeout", "9000");
        if ("infinity".equals(t)) {
            DEFAULT_TIMEOUT = RpcHelper.INFINITY;
        } else {
            try {
                DEFAULT_TIMEOUT = Integer.parseInt(t);
            } catch (final Exception e) {
                DEFAULT_TIMEOUT = 9000;
            }
        }
    }

    public void removePath(final String path) {
        codeManager.removePath(path);
    }

    public void addPath(final boolean usePathZ, final String path) {
        codeManager.addPath(usePathZ, path);
    }

    public synchronized void initErlang(final boolean monitor,
            final boolean watch) {
        init(getEventPid(), monitor, watch);

        // data.monitor = monitor;
        // data.managed = watch;

        eventDaemon = new ErlangEventPublisher(this);
        eventDaemon.start();
        new LogEventHandler(this).register();
        new ErlangLogEventHandler(this).register();

        BackendCore.getBackendManager().addBackendListener(
                eventDaemon.getBackendListener());
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
    public void addProjectPath(final IProject project) {
        final IErlProject eproject = ErlModelManager.getErlangModel()
                .findProject(project);
        final String outDir = project.getLocation()
                .append(eproject.getOutputLocation()).toOSString();
        if (outDir.length() > 0) {
            ErlLogger.debug("backend %s: add path %s", getName(), outDir);
            if (isDistributed()) {
                final boolean accessible = ErlideUtil
                        .isAccessible(this, outDir);
                if (accessible) {
                    addPath(false/* prefs.getUsePathZ() */, outDir);
                } else {
                    loadBeamsFromDir(outDir);
                }
            }
        }
    }

    @Override
    public void removeProjectPath(final IProject project) {
        final IErlProject eproject = ErlModelManager.getErlangModel()
                .findProject(project);
        if (eproject == null) {
            // can happen if project was removed
            return;
        }
        final String outDir = project.getLocation()
                .append(eproject.getOutputLocation()).toOSString();
        if (outDir.length() > 0) {
            ErlLogger.debug("backend %s: remove path %s", getName(), outDir);
            if (isDistributed()) {
                final boolean accessible = ErlideUtil
                        .isAccessible(this, outDir);
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
                            ok = BackendHelper.loadBeam(this, m, bin);
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
    public boolean doLoadOnAllNodes() {
        return getData().isLoadAllNodes();
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
            final boolean distributed = (data.getDebugFlags() & ErlDebugConstants.DISTRIBUTED_DEBUG) != 0;
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
            BackendCore.getBackendManager().addExecutionBackend(project, this);
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
                    cast(module, function, "s", args);
                } else {
                    cast(module, function, "");
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
        final OtpErlangList nodes = ErlideDebug.nodes(this);
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
        final String[] debuggerModules = { "erlide_dbg_debugged",
                "erlide_dbg_icmd", "erlide_dbg_idb", "erlide_dbg_ieval",
                "erlide_dbg_iload", "erlide_dbg_iserver", "erlide_int", "int" };
        final List<OtpErlangTuple> modules = new ArrayList<OtpErlangTuple>(
                debuggerModules.length);
        for (final String module : debuggerModules) {
            final OtpErlangBinary b = getDebuggerBeam(module);
            if (b != null) {
                final OtpErlangString filename = new OtpErlangString(module
                        + ".erl");
                final OtpErlangTuple t = OtpErlang.mkTuple(new OtpErlangAtom(
                        module), filename, b);
                modules.add(t);
            }
        }
        ErlideDebug.distributeDebuggerCode(this, modules);
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
                        return getBeamFromBundlePath(bundle, beamname, s, path);
                    }
                }
            }
        }
        return null;
    }

    private OtpErlangBinary getBeamFromBundlePath(final Bundle bundle,
            final String beamname, final String s, final Path path) {
        if (!path.lastSegment().equals(beamname)) {
            return null; // Shouln't happen
        }
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
            final IBackendManager bm = BackendCore.getBackendManager();
            for (final ICodeBundle bb : bm.getCodeBundles().values()) {
                registerCodeBundle(bb);
            }
            initErlang(data.isMonitored(), data.isManaged());

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
                    boolean b = ErlideDebug.interpret(this, beam.getLocation()
                            .toString(), distributed, interpret);
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

    @Override
    public void setMonitoring(final boolean on) {
        try {
            call("erlide_kernel_common", "set_monitoring", "o", on);
        } catch (final RpcException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void setMonitoringInterval(final int monitoringInterval) {
        try {
            call("erlide_kernel_common", "set_monitoring_interval", "i",
                    monitoringInterval);
        } catch (final RpcException e) {
            e.printStackTrace();
        }
    }

}

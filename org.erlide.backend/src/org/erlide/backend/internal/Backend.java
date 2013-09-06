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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;

import org.eclipse.core.resources.IProject;
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
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.jdt.annotation.NonNull;
import org.erlide.backend.BackendPlugin;
import org.erlide.backend.api.BackendData;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendManager;
import org.erlide.backend.api.ICodeBundle;
import org.erlide.backend.console.BackendShellManager;
import org.erlide.backend.debug.ErlideDebug;
import org.erlide.backend.debug.model.ErlangDebugNode;
import org.erlide.backend.debug.model.ErlangDebugTarget;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.api.BeamLoader;
import org.erlide.runtime.api.ErlDebugFlags;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.InitialCall;
import org.erlide.runtime.api.RuntimeUtils;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.runtime.shell.IoRequest.IoRequestKind;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;
import org.erlide.util.erlang.OtpErlang;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public abstract class Backend implements IStreamListener, IBackend {

    private final IErlRuntime runtime;
    private BackendShellManager shellManager;
    private final CodeManager codeManager;

    private final BackendData data;
    private ErlangDebugTarget debugTarget;
    protected final IBackendManager backendManager;

    public Backend(final BackendData data, @NonNull final IErlRuntime runtime,
            final IBackendManager backendManager) {
        assertThat(runtime, is(not(nullValue())));
        this.runtime = runtime;
        this.data = data;
        this.backendManager = backendManager;
        codeManager = new CodeManager(getRpcSite(), data.getRuntimeInfo()
                .getName());
    }

    @Override
    public void dispose() {
        if (data.isDebug()) {
            unloadDebuggerCode();
        }
        if (shellManager != null) {
            shellManager.dispose();
            shellManager = null;
        }
        runtime.dispose();
    }

    @Override
    public String getName() {
        return runtime.getNodeName();
    }

    protected boolean startErlideApps(final OtpErlangPid jRex,
            final boolean watch) {
        try {
            getRpcSite().call(
                    "erlide_common_app",
                    "init",
                    "poii",
                    jRex,
                    watch,
                    SystemConfiguration.getInstance()
                            .getWarnProcessSizeLimitMB(),
                    SystemConfiguration.getInstance()
                            .getKillProcessSizeLimitMB());
            // TODO should use extension point!
            getRpcSite().call("erlide_builder_app", "init", "");
            getRpcSite().call("erlide_ide_app", "init", "");

            // TODO start tracing when configured to do so!
            // getRpcSite().call("erlide_tracer", "start", "");
            return true;
        } catch (final Exception e) {
            ErlLogger.error(e);
            return false;
        }
    }

    @Override
    public boolean isRunning() {
        return runtime.isRunning();
    }

    public void removePath(final String path) {
        codeManager.removePath(path);
    }

    public void addPath(final boolean usePathZ, final String path) {
        codeManager.addPath(usePathZ, path);
    }

    public synchronized void initErlang(final boolean watch) {
        ErlLogger.debug("initialize %s: %s", getName(), watch);
        startErlideApps(getRuntime().getEventMbox().self(), watch);
        getRuntime().registerEventListener(new SystemMonitorHandler(getName()));
    }

    @Override
    public void registerCodeBundle(final ICodeBundle bundle) {
        codeManager.register(bundle);
    }

    @Override
    public void unregisterCodeBundle(final ICodeBundle b) {
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

    public void assignStreamProxyListeners() {
        if (data.getLaunch() == null) {
            return;
        }
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
    public void input(final String s) throws IOException {
        if (isRunning()) {
            final IStreamsProxy proxy = getStreamsProxy();
            if (proxy != null) {
                proxy.write(s);
            } else {
                ErlLogger
                        .warn("Could not send input to backend %s, stream proxy is null",
                                getName());
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
            final boolean accessible = RuntimeUtils.isAccessibleDir(
                    getRpcSite(), outDir);
            if (accessible) {
                addPath(false/* prefs.getUsePathZ() */, outDir);
            } else {
                loadBeamsFromDir(outDir);
            }
        }
    }

    @Override
    public void removeProjectPath(final IErlProject eproject) {
        if (eproject == null) {
            // can happen if project was removed
            return;
        }
        try {
            final IProject project = eproject.getWorkspaceProject();
            final String outDir = project.getLocation()
                    .append(eproject.getOutputLocation()).toOSString();
            if (outDir.length() > 0) {
                ErlLogger
                        .debug("backend %s: remove path %s", getName(), outDir);
                removePath(outDir);
                // TODO unloadBeamsFromDir(outDir); ?
            }
        } catch (final Exception e) {
            // can happen when shutting down
            ErlLogger.warn(e);
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

    protected IStreamsProxy getStreamsProxy() {
        return null;
    }

    protected void postLaunch() throws DebugException {
        final Collection<IProject> projects = Lists.newArrayList(data
                .getProjects());
        registerProjectsWithExecutionBackend(projects);
        if (data.isDebug()) {
            // add debug debugTarget
            final ILaunch launch = getData().getLaunch();
            debugTarget = new ErlangDebugTarget(launch, this, projects,
                    data.getDebugFlags());
            // debugTarget.getWaiter().doWait();
            launch.addDebugTarget(debugTarget);
            // interpret everything we can
            final boolean distributed = data.getDebugFlags().contains(
                    ErlDebugFlags.DISTRIBUTED_DEBUG);
            if (distributed) {
                distributeDebuggerCode();
                addNodesAsDebugTargets(launch, debugTarget);
            }
            debugTarget.interpretModules(data.getInitialInterpretedModules(),
                    distributed);
            registerStartupFunctionStarter(data);
            debugTarget.sendStarted();
        } else {
            final InitialCall initCall = data.getInitialCall();
            if (initCall != null) {
                runInitial(initCall.getModule(), initCall.getName(),
                        initCall.getParameters());
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
                        final InitialCall initCall = myData.getInitialCall();
                        if (initCall != null) {
                            runInitial(initCall.getModule(),
                                    initCall.getName(),
                                    initCall.getParameters());
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

    private void addNodesAsDebugTargets(final ILaunch aLaunch,
            final ErlangDebugTarget target) {
        final OtpErlangList nodes = ErlideDebug.nodes(getRpcSite());
        if (nodes != null) {
            for (int i = 1, n = nodes.arity(); i < n; ++i) {
                final OtpErlangAtom a = (OtpErlangAtom) nodes.elementAt(i);
                final IDebugTarget edn = new ErlangDebugNode(target,
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
        if (bundle == null) {
            ErlLogger.error("* !!! error bundle not found "
                    + "org.erlide.kernel.debugger");
            return null;
        }

        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] els = reg.getConfigurationElementsFor(
                BackendPlugin.PLUGIN_ID, "codepath");

        // TODO: this code assumes that the debugged debugTarget and the
        // erlide-plugin uses the same Erlang version, how can we escape this?

        for (final IConfigurationElement el : els) {
            final IContributor c = el.getContributor();
            final String name = c.getName();
            if (name.equals(bundle.getSymbolicName())) {
                final String dirPath = el.getAttribute("path");
                final Enumeration<?> e = bundle.getEntryPaths(dirPath);
                if (e == null) {
                    ErlLogger.error("* !!! error loading plugin "
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

    private OtpErlangBinary getBeamFromBundlePath(@NonNull final Bundle bundle,
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
    public BackendData getData() {
        return data;
    }

    @Override
    public void initialize(final Collection<ICodeBundle> bundles) {
        runtime.addShutdownCallback(this);
        shellManager = new BackendShellManager(this);
        for (final ICodeBundle bb : bundles) {
            registerCodeBundle(bb);
        }
        initErlang(data.isManaged());

        try {
            postLaunch();
        } catch (final DebugException e) {
            ErlLogger.error(e);
        }
    }

    // /////

    @Override
    public IRpcSite getRpcSite() {
        return runtime.getRpcSite();
    }

    @Override
    public RuntimeInfo getRuntimeInfo() {
        return runtime.getRuntimeData().getRuntimeInfo();
    }

    @Override
    public IErlRuntime getRuntime() {
        return runtime;
    }

    @Override
    public void onShutdown() {
    }
}

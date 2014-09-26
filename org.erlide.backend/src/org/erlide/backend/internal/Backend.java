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
import java.util.Collection;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.jdt.annotation.NonNull;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendUtils;
import org.erlide.backend.api.BackendData;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendManager;
import org.erlide.backend.api.ICodeBundle;
import org.erlide.backend.api.ICodeBundle.CodeContext;
import org.erlide.backend.console.BackendShellManager;
import org.erlide.backend.debug.BeamUtil;
import org.erlide.backend.debug.ErlideDebug;
import org.erlide.backend.debug.model.ErlangDebugTarget;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.api.BeamLoader;
import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.runtime.shell.IoRequest.IoRequestKind;
import org.erlide.util.ErlLogger;
import org.erlide.util.ErlangFunctionCall;
import org.erlide.util.SystemConfiguration;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.google.common.collect.Lists;

public abstract class Backend implements IStreamListener, IBackend {

    private final IOtpNodeProxy runtime;
    private BackendShellManager shellManager;
    private final CodeManager codeManager;

    private final BackendData data;
    private ErlangDebugTarget debugTarget;
    protected final IBackendManager backendManager;
    private boolean disposed = false;

    public Backend(final BackendData data, @NonNull final IOtpNodeProxy runtime,
            final IBackendManager backendManager) {
        this.runtime = runtime;
        this.data = data;
        this.backendManager = backendManager;
        codeManager = new CodeManager(getOtpRpc(), data.getRuntimeInfo().getName(), data
                .getRuntimeInfo().getVersion());
    }

    @Override
    public synchronized void dispose() {
        if (disposed) {
            return;
        }
        disposed = true;
        if (data.isDebug() && debugTarget != null) {
            debugTarget.dispose();
        }
        if (shellManager != null) {
            shellManager.dispose();
            shellManager = null;
        }
        runtime.dispose();
        BackendCore.getBackendManager().removeBackend(this);
    }

    @Override
    public String getName() {
        return runtime.getNodeName();
    }

    protected boolean startErlideApps(final OtpErlangPid jRex, final boolean watch) {
        try {
            final IOtpRpc site = getOtpRpc();
            site.call("erlide_common_app", "init", "poiii", jRex, watch,
                    SystemConfiguration.getInstance().getWarnProcessSizeLimitMB(),
                    SystemConfiguration.getInstance().getKillProcessSizeLimitMB(),
                    SystemConfiguration.getInstance().getMaxParallelBuilds());
            // TODO should use extension point!
            switch (data.getContext()) {
            case IDE:
                site.call("erlide_builder_app", "init", "");
                site.call("erlide_ide_app", "init", "");
                break;
            default:
            }
            // site.call("erlide_tracer", "start", "");
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

    public void removePath(final @NonNull String path) {
        codeManager.removePath(path);
    }

    public void addPath(final boolean usePathZ, final @NonNull String path) {
        codeManager.addPath(usePathZ, path);
    }

    public synchronized void initErlang(final boolean watch) {
        startErlideApps(getRuntime().getEventPid(), watch);
    }

    @Override
    public void registerCodeBundle(final CodeContext context, final ICodeBundle bundle) {
        codeManager.register(context, bundle);
    }

    @Override
    public void unregisterCodeBundle(final CodeContext context, final ICodeBundle b) {
        codeManager.unregister(context, b);
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
            final IStreamMonitor errorStreamMonitor = proxy.getErrorStreamMonitor();
            errorStreamMonitor.addListener(this);
            final IStreamMonitor outputStreamMonitor = proxy.getOutputStreamMonitor();
            outputStreamMonitor.addListener(this);
        }
    }

    @Override
    public IBackendShell getShell(final String id) {
        final IBackendShell shell = shellManager.openShell(id);
        final IStreamsProxy proxy = getStreamsProxy();
        if (proxy != null) {
            final IStreamMonitor errorStreamMonitor = proxy.getErrorStreamMonitor();
            errorStreamMonitor.addListener(new IStreamListener() {
                @Override
                public void streamAppended(final String text, final IStreamMonitor monitor) {
                    shell.add(text, IoRequestKind.STDERR);
                }
            });
            final IStreamMonitor outputStreamMonitor = proxy.getOutputStreamMonitor();
            outputStreamMonitor.addListener(new IStreamListener() {
                @Override
                public void streamAppended(final String text, final IStreamMonitor monitor) {
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
                ErlLogger.warn(
                        "Could not send input to backend %s, stream proxy is null",
                        getName());
            }
        }
    }

    @Override
    public void addProjectPath(final IErlProject eproject) {
        if (eproject == null) {
            return;
        }
        final IProject project = eproject.getWorkspaceProject();
        final String outDir = project.getLocation()
                .append(eproject.getProperties().getOutputDir()).toOSString();
        if (outDir.length() > 0) {
            final boolean accessible = BackendUtils.isAccessibleDir(getOtpRpc(), outDir);
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
                    .append(eproject.getProperties().getOutputDir()).toOSString();
            if (outDir.length() > 0) {
                removePath(outDir);
                // TODO unloadBeamsFromDir(outDir); ?
            }
        } catch (final Exception e) {
            // can happen when shutting down, ignore
        }
    }

    protected IStreamsProxy getStreamsProxy() {
        return null;
    }

    protected void postLaunch() throws DebugException {
        final Collection<IProject> projects = Lists.newArrayList(data.getProjects());
        registerProjectsWithExecutionBackend(projects);
        if (data.isDebug()) {
            // add debugTarget
            final ILaunch launch = getData().getLaunch();
            if (!debuggerIsRunning()) {
                debugTarget = new ErlangDebugTarget(launch, this, projects);
                launch.addDebugTarget(debugTarget);
                registerStartupFunctionStarter(data);
                debugTarget.sendStarted();
            }
        } else if (data.isManaged()) {
            // don't run this if the node is already running
            final ErlangFunctionCall initCall = data.getInitialCall();
            if (initCall != null) {
                runInitial(initCall.getModule(), initCall.getName(),
                        initCall.getParameters());
            }
        }
    }

    private boolean debuggerIsRunning() {
        return ErlideDebug.isRunning(getOtpRpc());
    }

    private void registerProjectsWithExecutionBackend(final Collection<IProject> projects) {
        for (final IProject project : projects) {
            backendManager.addExecutionBackend(project, this);
        }
    }

    private void registerStartupFunctionStarter(final BackendData myData) {
        DebugPlugin.getDefault().addDebugEventListener(new IDebugEventSetListener() {
            @Override
            public void handleDebugEvents(final DebugEvent[] events) {
                final ErlangFunctionCall initCall = myData.getInitialCall();
                if (initCall != null) {
                    runInitial(initCall.getModule(), initCall.getName(),
                            initCall.getParameters());
                }
                DebugPlugin.getDefault().removeDebugEventListener(this);
            }
        });
    }

    void runInitial(final String module, final String function, final String args) {
        try {
            if (module.length() > 0 && function.length() > 0) {
                ErlLogger.debug("calling startup function %s:%s", module, function);
                if (args.length() > 0) {
                    getOtpRpc().cast(module, function, "s", args);
                } else {
                    getOtpRpc().cast(module, function, "");
                }
            }
        } catch (final Exception e) {
            ErlLogger.debug("Could not run initial call %s:%s(\"%s\")", module, function,
                    args);
            ErlLogger.warn(e);
        }
    }

    @Override
    public BackendData getData() {
        return data;
    }

    @Override
    public void initialize(final CodeContext context,
            final Collection<ICodeBundle> bundles) {
        runtime.setShutdownCallback(this);
        shellManager = new BackendShellManager(this);
        for (final ICodeBundle bb : bundles) {
            registerCodeBundle(context, bb);
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
    public IOtpRpc getOtpRpc() {
        return runtime.getOtpRpc();
    }

    @Override
    public IOtpNodeProxy getRuntime() {
        return runtime;
    }

    @Override
    public void onShutdown() {
        dispose();
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
                        final OtpErlangBinary bin = BeamUtil.getBeamBinary(m, path);
                        if (bin != null) {
                            ok = BeamLoader.loadBeam(getOtpRpc(), m, bin);
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
    public boolean isDebugging() {
        try {
            return "debug".equals(getData().getLaunch().getLaunchMode());
        } catch (final Exception e) {
            return false;
        }
    }

}

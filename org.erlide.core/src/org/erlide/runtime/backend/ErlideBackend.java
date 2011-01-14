/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchesListener2;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IOldErlangProjectProperties;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.BackendShell;
import org.erlide.jinterface.backend.ErlBackend;
import org.erlide.jinterface.backend.IDisposable;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.console.IoRequest.IoRequestKind;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.internal.CodeManager;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangBinary;

/**
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public final class ErlideBackend extends Backend implements IDisposable,
        IStreamListener, ILaunchesListener2 {

    private final CodeManager codeManager;
    private IStreamsProxy proxy;

    private ILaunch launch;
    private boolean managed = false;

    public ErlideBackend(final RuntimeInfo info) throws BackendException {
        super(info);
        codeManager = new CodeManager(this);
    }

    @Override
    public void dispose() {
        try {
            if (launch != null) {
                launch.terminate();
            }
        } catch (final DebugException e) {
            e.printStackTrace();
        }
        dispose(false);
    }

    @Override
    public void dispose(final boolean restart) {
        ErlLogger.debug("disposing backend " + getName());
        super.dispose(restart);
    }

    @Override
    public synchronized void restart() {
        super.restart();
        codeManager.reRegisterBundles();
        // initErlang();
        // fixme eventdaemon
    }

    public void removePath(final String path) {
        codeManager.removePath(path);
    }

    public void addPath(final boolean usePathZ, final String path) {
        codeManager.addPath(usePathZ, path);
    }

    @Override
    public synchronized void initErlang(final boolean monitor,
            final boolean watch) {
        super.initErlang(monitor, watch);
        ErlangCore.getBackendManager().addBackendListener(getEventDaemon());
    }

    public void register(final CodeBundle bundle) {
        codeManager.register(bundle);
    }

    public void unregister(final Bundle b) {
        codeManager.unregister(b);
    }

    public void setTrapExit(final boolean contains) {
    }

    public void streamAppended(final String text, final IStreamMonitor monitor) {
        if (monitor == proxy.getOutputStreamMonitor()) {
            // System.out.println(getName() + " OUT " + text);
        } else if (monitor == proxy.getErrorStreamMonitor()) {
            // System.out.println(getName() + " ERR " + text);
        } else {
            // System.out.println("???" + text);
        }
    }

    public ILaunch getLaunch() {
        return launch;
    }

    public void setLaunch(final ILaunch launch2) {
        launch = launch2;
    }

    @Override
    public BackendShell getShell(final String id) {
        final BackendShell shell = super.getShell(id);
        if (proxy != null) {
            final IStreamMonitor errorStreamMonitor = proxy
                    .getErrorStreamMonitor();
            errorStreamMonitor.addListener(new IStreamListener() {
                public void streamAppended(final String text,
                        final IStreamMonitor monitor) {
                    shell.add(text, IoRequestKind.STDERR);
                }
            });
            final IStreamMonitor outputStreamMonitor = proxy
                    .getOutputStreamMonitor();
            outputStreamMonitor.addListener(new IStreamListener() {
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
        return !getInfo().getNodeName().equals("");
    }

    @Override
    public void input(final String s) throws IOException {
        if (!isStopped()) {
            if (proxy != null) {
                proxy.write(s);
            } else {
                ErlLogger
                        .warn("Could not load module on backend %s, stream proxy is null",
                                getInfo());
            }
        }
    }

    public void launchesTerminated(final ILaunch[] launches) {
        for (final ILaunch aLaunch : launches) {
            if (aLaunch == launch) {
                stop();
            }
        }
    }

    public void launchesAdded(final ILaunch[] launches) {
    }

    public void launchesChanged(final ILaunch[] launches) {
    }

    public void launchesRemoved(final ILaunch[] launches) {
    }

    public void setStreamsProxy(final IStreamsProxy streamsProxy) {
        proxy = streamsProxy;
        if (proxy != null) {
            final IStreamMonitor errorStreamMonitor = proxy
                    .getErrorStreamMonitor();
            errorStreamMonitor.addListener(this);
            final IStreamMonitor outputStreamMonitor = proxy
                    .getOutputStreamMonitor();
            outputStreamMonitor.addListener(this);
        }
    }

    public void addProjectPath(final IProject project) {
        final IOldErlangProjectProperties prefs = ErlangCore
                .getProjectProperties(project);
        final String outDir = project.getLocation()
                .append(prefs.getOutputDir()).toOSString();
        if (outDir.length() > 0) {
            ErlLogger.debug("backend %s: add path %s", getName(), outDir);
            if (isDistributed()) {
                final boolean accessible = ErlideUtil
                        .isAccessible(this, outDir);
                if (accessible) {
                    addPath(false/* prefs.getUsePathZ() */, outDir);
                } else {
                    loadBeamsFromDir(project, outDir);
                }
            } else {
                final File f = new File(outDir);
                final BackendManager backendManager = ErlangCore
                        .getBackendManager();
                for (final File file : f.listFiles()) {
                    String name = file.getName();
                    if (!name.endsWith(".beam")) {
                        continue;
                    }
                    name = name.substring(0, name.length() - 5);
                    try {
                        ErlideUtil.loadModuleViaInput(this, project, name);
                        backendManager.moduleLoaded(this, project, name);
                    } catch (final ErlModelException e) {
                        e.printStackTrace();
                    } catch (final IOException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
    }

    private void loadBeamsFromDir(final IProject project, final String outDir) {
        final File dir = new File(outDir);
        if (dir.isDirectory()) {
            final BackendManager backendManager = ErlangCore
                    .getBackendManager();
            for (final File f : dir.listFiles()) {
                final Path path = new Path(f.getPath());
                if (path.getFileExtension() != null
                        && "beam".compareTo(path.getFileExtension()) == 0) {
                    final String m = path.removeFileExtension().lastSegment();
                    try {
                        boolean ok = false;
                        final OtpErlangBinary bin = ErlideUtil.getBeamBinary(m,
                                path);
                        if (bin != null) {
                            ok = ErlBackend.loadBeam(this, m, bin);
                        }
                        if (!ok) {
                            ErlLogger.error("Could not load %s", m);
                        }
                        backendManager.moduleLoaded(this, project, m);
                    } catch (final Exception ex) {
                        ErlLogger.warn(ex);
                    }
                }
            }
        }
    }

    public boolean isManaged() {
        return managed;
    }

    public void setManaged(final boolean b) {
        managed = b;
    }

    public boolean doLoadOnAllNodes() {
        return getInfo().loadOnAllNodes();
    }
}

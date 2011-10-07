/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.internal.backend;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.erlide.core.CoreScope;
import org.erlide.core.ErlangCore;
import org.erlide.core.backend.BackendCore;
import org.erlide.core.backend.BackendData;
import org.erlide.core.backend.BackendException;
import org.erlide.core.backend.BackendUtils;
import org.erlide.core.backend.IBackend;
import org.erlide.core.backend.IBackendFactory;
import org.erlide.core.backend.IBackendListener;
import org.erlide.core.backend.IBackendManager;
import org.erlide.core.backend.ICodeBundle;
import org.erlide.core.backend.ICodeBundle.CodeContext;
import org.erlide.core.backend.IErlideBackendVisitor;
import org.erlide.core.backend.events.ErlangEventHandler;
import org.erlide.core.backend.events.ErlangEventPublisher;
import org.erlide.core.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.common.Tuple;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.rpc.IRpcCallSite;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.epmd.EpmdWatcher;
import org.erlide.jinterface.epmd.IEpmdListener;
import org.osgi.framework.Bundle;
import org.osgi.service.event.Event;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

public final class BackendManager implements IEpmdListener, IBackendManager {

    public enum BackendEvent {
        ADDED, REMOVED, MODULE_LOADED
    }

    private volatile IBackend ideBackend;
    private final Object ideBackendLock = new Object();
    private final Map<IProject, Set<IBackend>> executionBackends;
    private final Map<String, IBackend> buildBackends;
    final List<IBackendListener> listeners;
    private final Map<Bundle, ICodeBundle> codeBundles;

    private EpmdWatcher epmdWatcher;
    private final Set<IBackend> allBackends;
    private EpmdWatchJob epmdWatcherJob;
    private final BackendManagerLaunchListener launchListener;
    private final IBackendFactory factory;

    public BackendManager() {
        ideBackend = null;
        executionBackends = Maps.newHashMap();
        buildBackends = Maps.newHashMap();
        allBackends = Sets.newHashSet();
        listeners = Lists.newArrayList();
        codeBundles = Maps.newHashMap();

        tryStartEpmdProcess();
        startEpmdWatcher();

        launchListener = new BackendManagerLaunchListener(this, DebugPlugin
                .getDefault().getLaunchManager());
        factory = BackendCore.getBackendFactory();

        registerGlobalEventhandlers();
    }

    private void tryStartEpmdProcess() {
        final RuntimeInfo info = BackendCore.getRuntimeInfoManager()
                .getErlideRuntime();
        if (info == null || info.getOtpHome() == null) {
            ErlLogger.error("Trying to start with a null runtime info");
            return;
        }
        // FIXME the location of epmd is OS dependent... look for it!
        // Win: $OTP/erts-$VSN/bin/epmd
        // Lin: $OTP/bin/$TARGET/epmd
        // Others: ?
        final String[] cmdline = new String[] {
                info.getOtpHome() + "/bin/epmd", "-daemon" };
        try {
            Runtime.getRuntime().exec(cmdline);
            ErlLogger.info("Epmd started.");
        } catch (final IOException e) {
            // ErlLogger.info("Could not start epmd! " + e.getMessage());
        }
    }

    private void startEpmdWatcher() {
        epmdWatcher = new EpmdWatcher();
        epmdWatcherJob = new EpmdWatchJob(epmdWatcher);
        epmdWatcher.addEpmdListener(this);
        new EpmdWatchJob(epmdWatcher).schedule(100);
    }

    private void registerGlobalEventhandlers() {
        new ErlangEventHandler("*", null) {
            public void handleEvent(final Event event) {
                if (ErlangCore.hasFeatureEnabled("erlide.eventhandler.debug")) {
                    ErlLogger.info("erlang event : "
                            + ErlangEventPublisher.dumpEvent(event));
                }
            }
        }.register();
    }

    public IBackend createExecutionBackend(final BackendData data) {
        ErlLogger.debug("create execution backend " + data.getNodeName());
        final IBackend b = factory.createBackend(data);
        addBackend(b);
        notifyBackendChange(b, BackendEvent.ADDED, null, null);
        return b;
    }

    private void addBackend(final IBackend b) {
        synchronized (allBackends) {
            allBackends.add(b);
        }
    }

    public IBackend getBuildBackend(final IProject project)
            throws BackendException {
        final IErlProject erlProject = CoreScope.getModel().getErlangProject(
                project);
        if (erlProject == null) {
            return null;
        }
        final RuntimeInfo info = erlProject.getRuntimeInfo();
        if (info == null) {
            ErlLogger.info("Project %s has no runtime info, using ide",
                    project.getName());
            if (ideBackend == null) {
                throw new BackendException(
                        "IDE backend is not created - check configuration!");
            }
            return ideBackend;
        }
        final String version = info.getVersion().asMajor().toString();
        IBackend b = buildBackends.get(version);
        if (b == null) {
            b = factory.createBuildBackend(info);
            buildBackends.put(version, b);
            notifyBackendChange(b, BackendEvent.ADDED, null, null);
        }
        return b;
    }

    public IBackend getIdeBackend() {
        // System.out.println("GET ide" + Thread.currentThread());
        if (ideBackend == null) {
            synchronized (ideBackendLock) {
                if (ideBackend == null) {
                    ideBackend = factory.createIdeBackend();
                    notifyBackendChange(ideBackend, BackendEvent.ADDED, null,
                            null);
                }
            }
        }
        // System.out.println(">>> " + ideBackend);
        return ideBackend;
    }

    void notifyBackendChange(final IBackend b, final BackendEvent type,
            final IProject project, final String moduleName) {
        if (listeners == null) {
            return;
        }

        final Object[] copiedListeners = listeners.toArray();
        for (final Object element : copiedListeners) {
            final IBackendListener listener = (IBackendListener) element;
            switch (type) {
            case ADDED:
                listener.runtimeAdded(b);
                break;
            case REMOVED:
                listener.runtimeRemoved(b);
                break;
            case MODULE_LOADED:
                listener.moduleLoaded(b, project, moduleName);
            }
        }
    }

    public synchronized void updateNodeStatus(final String host,
            final Collection<String> started, final Collection<String> stopped) {
        for (final String b : started) {
            final String name = b + "@" + host;
            // ErlLogger.debug("(epmd) started: '%s'", name);
            remoteNodeStatus(name, true, null);
        }
        for (final String b : stopped) {
            final String name = b + "@" + host;
            // ErlLogger.debug("(epmd) stopped: '%s'", name);
            remoteNodeStatus(name, false, null);
        }
    }

    private void remoteNodeStatus(final String node, final boolean up,
            final Object info) {
        if (!up) {
            for (final Entry<IProject, Set<IBackend>> e : executionBackends
                    .entrySet()) {
                for (final IBackend be : e.getValue()) {
                    final String bnode = be.getRuntimeInfo().getNodeName();
                    if (RuntimeInfo.buildLocalNodeName(bnode, true)
                            .equals(node)) {
                        removeExecutionBackend(e.getKey(), be);
                        break;
                    }
                }
            }
        }
    }

    public synchronized void removeExecutionBackend(final IProject project,
            final IBackend b) {
        b.removeProjectPath(project);
        Set<IBackend> list = executionBackends.get(project);
        if (list == null) {
            list = Sets.newHashSet();
            executionBackends.put(project, list);
        }
        list.remove(b);
    }

    public void loadCodepathExtensions() {
        final IExtensionPoint exPnt = BackendUtils.getCodepathExtension();
        // TODO listen to changes to the registry!

        final IExtension[] extensions = exPnt.getExtensions();
        for (int e = 0; e < extensions.length; e++) {
            final IExtension extension = extensions[e];
            if (!extension.isValid()) {
                continue;
            }
            addCodeBundle(extension);
        }
    }

    private void addCodeBundle(final IExtension extension) {
        final String pluginId = extension.getContributor().getName();
        final Bundle plugin = Platform.getBundle(pluginId);

        final List<Tuple<String, CodeContext>> paths = Lists.newArrayList();
        final List<Tuple<String, String>> inits = Lists.newArrayList();
        for (final IConfigurationElement el : extension
                .getConfigurationElements()) {
            if ("beam_dir".equals(el.getName())) {
                final String dir = el.getAttribute("path");
                final String t = el.getAttribute("context").toUpperCase();
                final CodeContext type = Enum.valueOf(CodeContext.class, t);
                paths.add(new Tuple<String, CodeContext>(dir, type));
            } else if ("init".equals(el.getName())) {
                final String module = el.getAttribute("module");
                final String function = el.getAttribute("function");
                inits.add(new Tuple<String, String>(module, function));
            } else {
                ErlLogger
                        .error("Unknown code bundle element: %s", el.getName());
            }
        }
        addBundle(plugin, paths, inits);
    }

    public void addBundle(final Bundle b,
            final Collection<Tuple<String, CodeContext>> paths,
            final Collection<Tuple<String, String>> inits) {
        final ICodeBundle p = findBundle(b);
        if (p != null) {
            return;
        }
        final CodeBundleImpl pp = new CodeBundleImpl(b, paths, inits);
        getCodeBundles().put(b, pp);
        forEachBackend(new IErlideBackendVisitor() {
            public void visit(final IBackend bb) {
                bb.register(pp);
            }
        });
    }

    private ICodeBundle findBundle(final Bundle b) {
        return getCodeBundles().get(b);
    }

    public Map<Bundle, ICodeBundle> getCodeBundles() {
        return codeBundles;
    }

    public void forEachBackend(final IErlideBackendVisitor visitor) {
        for (final IBackend b : getAllBackends()) {
            visitor.visit(b);
        }
    }

    public IRpcCallSite getByName(final String nodeName) {
        final Collection<IBackend> list = getAllBackends();
        for (final IBackend b : list) {
            if (b.getName().equals(nodeName)) {
                return b;
            }
        }
        return null;
    }

    public Collection<IBackend> getAllBackends() {
        synchronized (allBackends) {
            return Collections.unmodifiableCollection(allBackends);
        }
    }

    public void moduleLoaded(final IBackend b, final IProject project,
            final String moduleName) {
        notifyBackendChange(b, BackendEvent.MODULE_LOADED, project, moduleName);
    }

    public synchronized Set<IBackend> getExecutionBackends(
            final IProject project) {
        final Set<IBackend> bs = executionBackends.get(project);
        if (bs == null) {
            return Collections.emptySet();
        }
        return Collections.unmodifiableSet(bs);
    }

    public void addBackendListener(final IBackendListener listener) {
        listeners.add(listener);
    }

    public void removeBackendListener(final IBackendListener listener) {
        listeners.remove(listener);
    }

    public synchronized void addExecutionBackend(final IProject project,
            final IBackend b) {
        Set<IBackend> list = executionBackends.get(project);
        if (list == null) {
            list = Sets.newHashSet();
            executionBackends.put(project, list);
        }
        list.add(b);
        b.addProjectPath(project);
    }

    public EpmdWatcher getEpmdWatcher() {
        return epmdWatcher;
    }

    public void dispose(final IBackend backend) {
        if (backend != null && backend != ideBackend) {
            backend.dispose();
        }
    }

    public void dispose() {
        final ILaunch[] launches = DebugPlugin.getDefault().getLaunchManager()
                .getLaunches();
        launchListener.launchesTerminated(launches);
        launchListener.dispose();
        epmdWatcherJob.stop();
    }

    public IBackend getBackendForLaunch(final ILaunch launch) {
        for (final IBackend backend : allBackends) {
            if (backend.getLaunch() == launch) {
                return backend;
            }
        }
        return null;
    }

    public void terminateBackendsForLaunch(final ILaunch launch) {
    }

    public void removeBackendsForLaunch(final ILaunch launch) {
    }

}

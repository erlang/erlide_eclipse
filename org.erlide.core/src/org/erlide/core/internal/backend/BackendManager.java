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

import java.io.File;
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
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.erlide.core.CoreScope;
import org.erlide.core.backend.BackendCore;
import org.erlide.core.backend.BackendData;
import org.erlide.core.backend.BackendException;
import org.erlide.core.backend.BackendUtils;
import org.erlide.core.backend.IBackend;
import org.erlide.core.backend.IBackendListener;
import org.erlide.core.backend.ICodeBundle;
import org.erlide.core.backend.ICodeBundle.CodeContext;
import org.erlide.core.backend.IErlideBackendVisitor;
import org.erlide.core.backend.manager.BackendManagerLaunchListener;
import org.erlide.core.backend.manager.IBackendFactory;
import org.erlide.core.backend.manager.IBackendManager;
import org.erlide.core.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.common.Tuple;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.rpc.IRpcCallSite;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.epmd.EpmdWatcher;
import org.erlide.jinterface.epmd.IEpmdListener;
import org.osgi.framework.Bundle;

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

    private final EpmdWatcher epmdWatcher;
    private final Set<IBackend> allBackends;
    private final EpmdWatchJob epmdWatcherJob;
    private final BackendManagerLaunchListener launchListener;
    private final IBackendFactory factory;

    public BackendManager() {
        ideBackend = null;
        executionBackends = Maps.newHashMap();
        buildBackends = Maps.newHashMap();
        allBackends = Sets.newHashSet();
        listeners = Lists.newArrayList();
        codeBundles = Maps.newHashMap();

        // ManagedLauncher.startEpmdProcess();
        epmdWatcher = new EpmdWatcher();
        epmdWatcherJob = new EpmdWatchJob(epmdWatcher);
        epmdWatcher.addEpmdListener(this);
        new EpmdWatchJob(epmdWatcher).schedule(100);

        launchListener = new BackendManagerLaunchListener(this, DebugPlugin
                .getDefault().getLaunchManager());
        factory = BackendCore.getBackendFactory();

        // TODO remove this when all users have cleaned up
        cleanupInternalLCs();
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

    public synchronized Set<IBackend> getExecutionBackends(
            final IProject project) {
        final Set<IBackend> bs = executionBackends.get(project);
        if (bs == null) {
            return Collections.emptySet();
        }
        return Collections.unmodifiableSet(bs);
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

    public void addBackendListener(final IBackendListener listener) {
        listeners.add(listener);
    }

    public void removeBackendListener(final IBackendListener listener) {
        listeners.remove(listener);
    }

    public Collection<IBackend> getAllBackends() {
        synchronized (allBackends) {
            return Collections.unmodifiableCollection(allBackends);
        }
    }

    private void addCodeBundle(final IExtension extension) {
        final String pluginId = extension.getContributor().getName();
        final Bundle plugin = Platform.getBundle(pluginId);

        final List<Tuple<String, CodeContext>> paths = Lists.newArrayList();
        Tuple<String, String> init = null;
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
                init = new Tuple<String, String>(module, function);
            } else {
                ErlLogger
                        .error("Unknown code bundle element: %s", el.getName());
            }
        }
        addBundle(plugin, paths, init);
    }

    public void addBundle(final Bundle b,
            final Collection<Tuple<String, CodeContext>> paths,
            final Tuple<String, String> init) {
        final ICodeBundle p = findBundle(b);
        if (p != null) {
            return;
        }
        final CodeBundleImpl pp = new CodeBundleImpl(b, paths, init);
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

    public void forEachBackend(final IErlideBackendVisitor visitor) {
        for (final IBackend b : getAllBackends()) {
            visitor.visit(b);
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

    public EpmdWatcher getEpmdWatcher() {
        return epmdWatcher;
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

    public void dispose(final IBackend backend) {
        if (backend != null && backend != ideBackend) {
            backend.dispose();
        }
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

    public IRpcCallSite getByName(final String nodeName) {
        final Collection<IBackend> list = getAllBackends();
        for (final IBackend b : list) {
            if (b.getName().equals(nodeName)) {
                return b;
            }
        }
        return null;
    }

    private void cleanupInternalLCs() {
        final ILaunchManager lm = DebugPlugin.getDefault().getLaunchManager();
        try {
            final ILaunchConfiguration[] cfgs = lm.getLaunchConfigurations();
            int n = 0;
            for (final ILaunchConfiguration cfg : cfgs) {
                final String name = cfg.getName();
                if (name.startsWith("internal")) {
                    @SuppressWarnings("deprecation")
                    final IPath path = cfg.getLocation();
                    final File file = new File(path.toString());
                    file.delete();
                    n++;
                }
            }
            ErlLogger.debug("Cleaned up %d old LCs", n);
        } catch (final Exception e) {
            // ignore
        }
    }

    public void moduleLoaded(final IBackend b, final IProject project,
            final String moduleName) {
        notifyBackendChange(b, BackendEvent.MODULE_LOADED, project, moduleName);
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
        // TODO Auto-generated method stub

    }

    public void removeBackendsForLaunch(final ILaunch launch) {
        // TODO Auto-generated method stub

    }

    public IBackend createExecutionBackend(final BackendData data) {
        ErlLogger.debug("create execution backend " + data.getNodeName());
        final IBackend b = factory.createBackend(data);
        addBackend(b);
        notifyBackendChange(b, BackendEvent.ADDED, null, null);
        return b;
    }

    public void dispose() {
        final ILaunch[] launches = DebugPlugin.getDefault().getLaunchManager()
                .getLaunches();
        launchListener.launchesTerminated(launches);
        launchListener.dispose();
        epmdWatcherJob.stop();
    }

    public Map<Bundle, ICodeBundle> getCodeBundles() {
        return codeBundles;
    }
}

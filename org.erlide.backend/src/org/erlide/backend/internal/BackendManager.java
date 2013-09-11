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
package org.erlide.backend.internal;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendUtils;
import org.erlide.backend.api.BackendData;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendFactory;
import org.erlide.backend.api.IBackendListener;
import org.erlide.backend.api.IBackendManager;
import org.erlide.backend.api.ICodeBundle;
import org.erlide.backend.api.ICodeBundle.CodeContext;
import org.erlide.backend.api.IPluginCodeLoader;
import org.erlide.backend.api.IProjectCodeLoader;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.ErlLogger;
import org.osgi.framework.Bundle;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

public final class BackendManager implements IBackendManager {

    public enum BackendEvent {
        ADDED, REMOVED, MODULE_LOADED
    }

    private volatile IBackend ideBackend;
    private final Object ideBackendLock = new Object();
    private final Map<IProject, Set<IBackend>> executionBackends;
    private final Map<String, IBackend> buildBackends;
    final List<IBackendListener> listeners;
    private final Map<Bundle, ICodeBundle> codeBundles;

    private final Set<IBackend> allBackends;
    private final BackendManagerLaunchListener launchListener;
    private final IBackendFactory factory;
    private final RuntimeInfo erlideRuntimeInfo;

    public BackendManager(final RuntimeInfo erlideRuntimeInfo,
            final IBackendFactory factory) {
        this.factory = factory;
        this.erlideRuntimeInfo = erlideRuntimeInfo;

        ideBackend = null;
        executionBackends = Maps.newHashMap();
        buildBackends = Maps.newHashMap();
        allBackends = Sets.newHashSet();
        listeners = Lists.newArrayList();
        codeBundles = Maps.newHashMap();

        loadCodepathExtensions();

        launchListener = new BackendManagerLaunchListener(this, DebugPlugin
                .getDefault().getLaunchManager());
    }

    @SuppressWarnings("unused")
    private void tryStartEpmdProcess() {
        final RuntimeInfo info = erlideRuntimeInfo;
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

    @Override
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

    @Override
    public IBackend getBuildBackend(final IProject project) {
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(project);
        if (erlProject == null) {
            ErlLogger.warn("Project %s is not an erlang project",
                    project.getName());
            return null;
        }
        final RuntimeInfo info = erlProject.getRuntimeInfo();
        if (info == null) {
            ErlLogger.info("Project %s has no runtime info, using ide",
                    project.getName());
            return ideBackend;
        }
        final String version = info.getVersion().asMajor().toString();
        IBackend b = buildBackends.get(version);
        if (b == null) {
            b = factory.createBuildBackend(info);
            buildBackends.put(version, b);
            addBackend(b);
            notifyBackendChange(b, BackendEvent.ADDED, null, null);
        }
        return b;
    }

    @Override
    public IBackend getIdeBackend() {
        // System.out.println("GET ide" + Thread.currentThread());
        if (ideBackend == null) {
            synchronized (ideBackendLock) {
                if (ideBackend == null) {
                    ideBackend = factory.createIdeBackend();
                    addBackend(ideBackend);
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
                break;
            default:
                // ignore
            }
        }
    }

    @Override
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
                    final String bnode = be.getData().getQualifiedNodeName();
                    // TODO this is not very reliable
                    // we have to get the real node name... how?
                    if (bnode.startsWith(node)) {
                        removeExecutionBackend(e.getKey(), be);
                        break;
                    }
                }
            }
        }
    }

    @Override
    public synchronized void removeExecutionBackend(final IProject project,
            final IProjectCodeLoader b) {
        final IErlModel model = ErlangEngine.getInstance().getModel();
        b.removeProjectPath(model.findProject(project));
        Set<IBackend> list = executionBackends.get(project);
        if (list == null) {
            list = Sets.newHashSet();
            executionBackends.put(project, list);
        }
        list.remove(b);
    }

    @Override
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

        final Map<String, CodeContext> paths = Maps.newHashMap();
        final List<Pair<String, String>> inits = Lists.newArrayList();
        for (final IConfigurationElement el : extension
                .getConfigurationElements()) {
            if ("beam_dir".equals(el.getName())) {
                final String dir = el.getAttribute("path");
                final String t = el.getAttribute("context").toUpperCase();
                final CodeContext type = Enum.valueOf(CodeContext.class, t);
                paths.put(dir, type);
            } else if ("init".equals(el.getName())) {
                final String module = el.getAttribute("module");
                final String function = el.getAttribute("function");
                inits.add(new Pair<String, String>(module, function));
            } else {
                ErlLogger
                        .error("Unknown code bundle element: %s", el.getName());
            }
        }
        addBundle(plugin, paths, inits);
    }

    private void addBundle(final Bundle b,
            final Map<String, CodeContext> paths,
            final Collection<Pair<String, String>> inits) {
        final ICodeBundle p = findBundle(b);
        if (p != null) {
            return;
        }
        final CodeBundle pp = new CodeBundle(b, paths, inits);
        getCodeBundles().put(b, pp);
        forEachBackend(new Procedure1<IBackend>() {
            @Override
            public void apply(final IBackend bb) {
                bb.registerCodeBundle(pp);
            }
        });
    }

    private ICodeBundle findBundle(final Bundle b) {
        return getCodeBundles().get(b);
    }

    @Override
    public Map<Bundle, ICodeBundle> getCodeBundles() {
        return codeBundles;
    }

    @Override
    public void forEachBackend(final Procedure1<IBackend> visitor) {
        for (final IBackend b : getAllBackends()) {
            visitor.apply(b);
        }
    }

    @Override
    public IRpcSite getByName(final String nodeName) {
        final Collection<IBackend> list = getAllBackends();
        for (final IBackend b : list) {
            if (b.getName().equals(nodeName)) {
                return b.getRpcSite();
            }
        }
        return null;
    }

    @Override
    public IRpcSite getByVersion(final RuntimeVersion version) {
        final RuntimeInfo info = BackendCore.getRuntimeInfoCatalog()
                .getRuntime(version, null);
        if (info == null) {
            return null;
        }
        final Collection<IBackend> list = getAllBackends();
        for (final IBackend b : list) {
            if (b.getRuntimeInfo().getVersion().equals(version)) {
                return b.getRpcSite();
            }
        }
        for (final IBackend b : list) {
            if (b.getRuntimeInfo().getVersion().isCompatible(version)) {
                return b.getRpcSite();
            }
        }
        return null;
    }

    @Override
    public IRpcSite getByProject(final String projectName) {
        final IProject project = ResourcesPlugin.getWorkspace().getRoot()
                .getProject(projectName);
        final IBackend backend = getBuildBackend(project);
        if (backend == null) {
            ErlLogger.debug("Could not find backend for project %S", project);
            return null;
        }
        return backend.getRpcSite();
    }

    @Override
    public Collection<IBackend> getAllBackends() {
        synchronized (allBackends) {
            return Collections.unmodifiableCollection(allBackends);
        }
    }

    @Override
    public void moduleLoaded(final IBackend b, final IProject project,
            final String moduleName) {
        notifyBackendChange(b, BackendEvent.MODULE_LOADED, project, moduleName);
    }

    @Override
    public synchronized Set<IBackend> getExecutionBackends(
            final IProject project) {
        final Set<IBackend> bs = executionBackends.get(project);
        if (bs == null) {
            return Collections.emptySet();
        }
        return Collections.unmodifiableSet(bs);
    }

    @Override
    public void addBackendListener(final IBackendListener listener) {
        listeners.add(listener);
    }

    @Override
    public void removeBackendListener(final IBackendListener listener) {
        listeners.remove(listener);
    }

    @Override
    public synchronized void addExecutionBackend(final IProject project,
            final IBackend b) {
        Set<IBackend> list = executionBackends.get(project);
        if (list == null) {
            list = Sets.newHashSet();
            executionBackends.put(project, list);
        }
        list.add(b);
        final IErlModel model = ErlangEngine.getInstance().getModel();
        b.addProjectPath(model.findProject(project));
    }

    @Override
    public void dispose() {
        for (final IBackend b : buildBackends.values()) {
            b.dispose();
        }
        if (ideBackend != null) {
            ideBackend.dispose();
        }
        final ILaunch[] launches = DebugPlugin.getDefault().getLaunchManager()
                .getLaunches();
        launchListener.launchesTerminated(launches);
        launchListener.dispose();
    }

    @Override
    public IBackend getBackendForLaunch(final ILaunch launch) {
        for (final IBackend backend : allBackends) {
            if (backend.getData().getLaunch() == launch) {
                return backend;
            }
        }
        return null;
    }

    @Override
    public void terminateBackendsForLaunch(final ILaunch launch) {
        final IBackend b = getBackendForLaunch(launch);
        if (b != null) {
            b.dispose();
        }
    }

    @Override
    public void removeBackendsForLaunch(final ILaunch launch) {
    }

    @Override
    public IPluginCodeLoader getByProcess(final IProcess process) {
        synchronized (allBackends) {
            for (final IBackend backend : allBackends) {
                final ILaunch launch = backend.getData().getLaunch();
                if (launch == null) {
                    continue;
                }
                final IProcess[] processes = launch.getProcesses();
                if (processes.length > 0 && processes[0].equals(process)) {
                    return backend;
                }
            }
            return null;
        }
    }

}

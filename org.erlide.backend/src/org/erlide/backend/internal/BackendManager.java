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
import org.eclipse.jdt.annotation.NonNull;
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
import org.erlide.backend.api.IProjectCodeLoader;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.ErlLogger;
import org.osgi.framework.Bundle;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import com.google.common.collect.Sets;

public final class BackendManager implements IBackendManager {

    public enum BackendEvent {
        ADDED, REMOVED, MODULE_LOADED
    }

    private IBackend ideBackend;
    private final Map<IProject, Set<IBackend>> executionBackends;
    private final Map<String, IBackend> buildBackends;
    final List<IBackendListener> listeners;
    private final Map<Bundle, ICodeBundle> codeBundles;

    private final Set<IBackend> allBackends;
    private final BackendManagerLaunchListener launchListener;
    private final IBackendFactory factory;

    public BackendManager(final IBackendFactory factory) {
        this.factory = factory;

        ideBackend = null;
        executionBackends = Maps.newHashMap();
        buildBackends = Maps.newHashMap();
        allBackends = Sets.newHashSet();
        listeners = Lists.newArrayList();
        codeBundles = Maps.newHashMap();

        loadCodepathExtensions();

        launchListener = new BackendManagerLaunchListener(this, DebugPlugin.getDefault()
                .getLaunchManager());
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
    public IBackend getBuildBackend(@NonNull final IErlProject project) {
        final RuntimeInfo info = project.getRuntimeInfo();
        if (info == null) {
            ErlLogger
                    .info("Project %s has no runtime info, using ide", project.getName());
            return getIdeBackend();
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
    public synchronized IBackend getIdeBackend() {
        if (ideBackend == null) {
            final IBackend result = factory.createIdeBackend();
            addBackend(result);
            notifyBackendChange(result, BackendEvent.ADDED, null, null);
            ideBackend = result;
        }
        return ideBackend;
    }

    void notifyBackendChange(final IBackend b, final BackendEvent type,
            final IProject project, final String moduleName) {
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

    private void remoteNodeStatus(final String node, final boolean up, final Object info) {
        if (!up) {
            for (final Entry<IProject, Set<IBackend>> e : executionBackends.entrySet()) {
                for (final IBackend be : e.getValue()) {
                    final String bnode = be.getData().getQualifiedNodeName();
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

        final Multimap<CodeContext, String> paths = HashMultimap.create();
        final List<Pair<String, String>> inits = Lists.newArrayList();
        RuntimeVersion ver = RuntimeVersion.NO_VERSION;

        for (final IConfigurationElement el : extension.getConfigurationElements()) {
            if ("beam_dir".equals(el.getName())) {
                final String dir = el.getAttribute("path");
                final String t = el.getAttribute("context").toUpperCase();

                final CodeContext type = Enum.valueOf(CodeContext.class, t);
                paths.put(type, dir);
            } else if ("init".equals(el.getName())) {
                final String module = el.getAttribute("module");
                final String function = el.getAttribute("function");
                inits.add(new Pair<String, String>(module, function));
            } else if ("otp_version".equals(el.getName())) {
                final String attribute = el.getAttribute("value");
                if (attribute != null) {
                    ver = RuntimeVersion.Serializer.parse(attribute);
                }
            } else {
                ErlLogger.error("Unknown code bundle element: %s", el.getName());
            }
        }
        addBundle(plugin, ver, paths, inits);
    }

    private void addBundle(final Bundle b, final RuntimeVersion version,
            final Multimap<CodeContext, String> paths,
            final Collection<Pair<String, String>> inits) {
        final ICodeBundle p = codeBundles.get(b);
        if (p != null) {
            return;
        }
        final CodeBundle pp = new CodeBundle(b, version, paths, inits);
        codeBundles.put(b, pp);
    }

    @Override
    public Collection<ICodeBundle> getCodeBundles() {
        return codeBundles.values();
    }

    @Override
    public void forEachBackend(final Procedure1<IBackend> visitor) {
        for (final IBackend b : getAllBackends()) {
            visitor.apply(b);
        }
    }

    @Override
    public IOtpRpc getByName(final String nodeName) {
        final Collection<IBackend> list = getAllBackends();
        for (final IBackend b : list) {
            if (b.getName().equals(nodeName)) {
                return b.getOtpRpc();
            }
        }
        return null;
    }

    @Override
    public IOtpRpc getByVersion(final RuntimeVersion version) {
        final RuntimeInfo info = BackendCore.getRuntimeInfoCatalog().getRuntime(version,
                null);
        if (info == null) {
            return null;
        }
        final Collection<IBackend> list = getAllBackends();
        for (final IBackend b : list) {
            if (b.getRuntime().getVersion().equals(version)) {
                return b.getOtpRpc();
            }
        }
        for (final IBackend b : list) {
            if (b.getRuntime().getVersion().isCompatible(version)) {
                return b.getOtpRpc();
            }
        }
        return null;
    }

    @Override
    public IOtpRpc getByProject(final String projectName) {
        final IProject project = ResourcesPlugin.getWorkspace().getRoot()
                .getProject(projectName);
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(project);
        if (erlProject == null) {
            return null;
        }

        final IBackend backend = getBuildBackend(erlProject);
        if (backend == null) {
            ErlLogger.warn("Could not find backend for project %s", project);
            return null;
        }
        return backend.getOtpRpc();
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
    public synchronized Set<IBackend> getExecutionBackends(final @NonNull IProject project) {
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
    public synchronized void addExecutionBackend(final IProject project, final IBackend b) {
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
        synchronized (this) {
            final Collection<IBackend> bb = Lists.newArrayList(buildBackends.values());
            buildBackends.clear();
            for (final IBackend b : bb) {
                b.dispose();
            }
            if (ideBackend != null) {
                ideBackend.dispose();
                ideBackend = null;
            }
            final ILaunch[] launches = DebugPlugin.getDefault().getLaunchManager()
                    .getLaunches();
            launchListener.launchesTerminated(launches);
            launchListener.dispose();
        }
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
    public synchronized void removeBackend(final IBackend backend) {
        allBackends.remove(backend);
        if (buildBackends.values().contains(backend)) {
            buildBackends.values().remove(backend);
        }
    }

    @Override
    public IBackendFactory getFactory() {
        return this.factory;
    }
}

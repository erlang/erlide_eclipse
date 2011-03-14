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
package org.erlide.core.backend.manager;

import java.io.File;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.erlide.core.ErlangCore;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.backend.Backend;
import org.erlide.core.backend.BackendData;
import org.erlide.core.backend.BackendException;
import org.erlide.core.backend.BackendListener;
import org.erlide.core.backend.BackendOptions;
import org.erlide.core.backend.CodeBundle;
import org.erlide.core.backend.ErlLaunchAttributes;
import org.erlide.core.backend.ErlideBackendVisitor;
import org.erlide.core.backend.launching.ErlangLaunchDelegate;
import org.erlide.core.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.common.Tuple;
import org.erlide.core.internal.backend.BackendUtil;
import org.erlide.core.internal.backend.CodeBundleImpl;
import org.erlide.core.internal.backend.CodeBundleImpl.CodeContext;
import org.erlide.core.internal.backend.EpmdWatchJob;
import org.erlide.core.model.erlang.IErlProject;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.epmd.EpmdWatcher;
import org.erlide.jinterface.epmd.IEpmdListener;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpNodeStatus;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

public final class BackendManager extends OtpNodeStatus implements
        IEpmdListener {

    public enum BackendEvent {
        ADDED, REMOVED, MODULE_LOADED
    }

    private volatile Backend ideBackend;
    private final Object ideBackendLock = new Object();
    private final Map<IProject, Set<Backend>> executionBackends;
    private final Map<String, Backend> buildBackends;
    final List<BackendListener> listeners;
    private final Map<Bundle, CodeBundleImpl> codeBundles;

    private final EpmdWatcher epmdWatcher;
    private final Set<Backend> allBackends;
    private final EpmdWatchJob epmdWatcherJob;
    private final BackendManagerLaunchListener launchListener;
    private final BackendFactory factory;

    @SuppressWarnings("synthetic-access")
    private static final class LazyBackendManagerHolder {
        public static final BackendManager instance = new BackendManager();
    }

    public static final BackendManager getDefault() {
        return LazyBackendManagerHolder.instance;
    }

    private BackendManager() {
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
        factory = new BackendFactory();

        // TODO remove this when all users have cleaned up
        cleanupInternalLCs();
    }

    private void addBackend(final Backend b) {
        synchronized (allBackends) {
            allBackends.add(b);
        }
    }

    public Backend getBuildBackend(final IProject project)
            throws BackendException {
        final IErlProject erlProject = ErlangCore.getModel().getErlangProject(
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
            ideBackend.addProjectPath(project);
            return ideBackend;
        }
        final String version = info.getVersion().asMajor().toString();
        Backend b = buildBackends.get(version);
        if (b == null) {
            b = factory.createBuildBackend(info);
            buildBackends.put(version, b);
            notifyBackendChange(b, BackendEvent.ADDED, null, null);
        }
        b.addProjectPath(project);
        return b;
    }

    private ILaunchConfiguration getLaunchConfiguration(final RuntimeInfo info,
            final Set<BackendOptions> options) {
        final ILaunchManager manager = DebugPlugin.getDefault()
                .getLaunchManager();
        final ILaunchConfigurationType type = manager
                .getLaunchConfigurationType(ErlangLaunchDelegate.CONFIGURATION_TYPE_INTERNAL);
        ILaunchConfigurationWorkingCopy workingCopy;
        try {
            final String name = getLaunchName(info, options);
            workingCopy = type.newInstance(null, name);
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
            if (System.getProperty("erlide.internal.shortname", "false")
                    .equals("true")) {
                workingCopy.setAttribute(ErlLaunchAttributes.USE_LONG_NAME,
                        false);
                info.useLongName(false);
            }
            return workingCopy;
        } catch (final CoreException e) {
            e.printStackTrace();
            return null;
        }
    }

    private String getLaunchName(final RuntimeInfo info,
            final Set<BackendOptions> options) {
        return "internal_" + info.getNodeName();
    }

    public synchronized Set<Backend> getExecutionBackends(final IProject project) {
        final Set<Backend> bs = executionBackends.get(project);
        if (bs == null) {
            return Collections.emptySet();
        }
        return Collections.unmodifiableSet(bs);
    }

    public Backend getIdeBackend() {
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

    public void addBackendListener(final BackendListener listener) {
        listeners.add(listener);
    }

    public void removeBackendListener(final BackendListener listener) {
        listeners.remove(listener);
    }

    public Collection<Backend> getAllBackends() {
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
        final CodeBundle p = findBundle(b);
        if (p != null) {
            return;
        }
        final CodeBundleImpl pp = new CodeBundleImpl(b, paths, init);
        getCodeBundles().put(b, pp);
        forEachBackend(new ErlideBackendVisitor() {
            public void visit(final Backend bb) {
                bb.register(pp);
            }
        });
    }

    private CodeBundle findBundle(final Bundle b) {
        return getCodeBundles().get(b);
    }

    public void forEachBackend(final ErlideBackendVisitor visitor) {
        for (final Backend b : getAllBackends()) {
            visitor.visit(b);
        }
    }

    public synchronized void updateNodeStatus(final String host,
            final Collection<String> started, final Collection<String> stopped) {
        for (final String b : started) {
            final String name = b + "@" + host;
            // ErlLogger.debug("(epmd) started: '%s'", name);
            remoteStatus(name, true, null);
        }
        for (final String b : stopped) {
            final String name = b + "@" + host;
            // ErlLogger.debug("(epmd) stopped: '%s'", name);
            remoteStatus(name, false, null);
        }

    }

    public synchronized void addExecutionBackend(final IProject project,
            final Backend b) {
        Set<Backend> list = executionBackends.get(project);
        if (list == null) {
            list = Sets.newHashSet();
            executionBackends.put(project, list);
        }
        list.add(b);
        b.addProjectPath(project);
    }

    public synchronized void removeExecutionBackend(final IProject project,
            final Backend b) {
        b.removeProjectPath(project);
        Set<Backend> list = executionBackends.get(project);
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
            for (final Entry<IProject, Set<Backend>> e : executionBackends
                    .entrySet()) {
                for (final Backend be : e.getValue()) {
                    final String bnode = be.getRuntimeInfo().getNodeName();
                    if (BackendUtil.buildLocalNodeName(bnode, true)
                            .equals(node)) {
                        removeExecutionBackend(e.getKey(), be);
                        break;
                    }
                }
            }
        }
    }

    public void dispose(final Backend backend) {
        if (backend != null && backend != ideBackend) {
            backend.dispose();
        }
    }

    @Override
    public void remoteStatus(final String node, final boolean up,
            final Object info) {
        // final String dir = up ? "up" : "down";
        // ErlLogger.debug(String.format("@@: %s %s %s", node, dir, info));
        remoteNodeStatus(node, up, info);
    }

    void notifyBackendChange(final Backend b, final BackendEvent type,
            final IProject project, final String moduleName) {
        if (listeners == null) {
            return;
        }

        final Object[] copiedListeners = listeners.toArray();
        for (final Object element : copiedListeners) {
            final BackendListener listener = (BackendListener) element;
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
        final IExtensionPoint exPnt = ErlangPlugin.getCodepathExtension();
        // TODO listen to changes to the registry!

        final IExtension[] extensions = exPnt.getExtensions();
        for (int e = 0; e < extensions.length; e++) {
            final IExtension extension = extensions[e];
            if (!extension.isValid()) {
                continue;
            }
            ErlangCore.getBackendManager().addCodeBundle(extension);
        }
    }

    public Backend getByName(final String nodeName) {
        final Collection<Backend> list = getAllBackends();
        for (final Backend b : list) {
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

    public void moduleLoaded(final Backend b, final IProject project,
            final String moduleName) {
        notifyBackendChange(b, BackendEvent.MODULE_LOADED, project, moduleName);
    }

    public Backend getBackendForLaunch(final ILaunch launch) {
        for (final Backend backend : allBackends) {
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

    public void createExecutionBackend(final BackendData data) {
        ErlLogger.debug("create execution backend " + data.getNodeName());
        final Backend b = factory.createBackend(data);
        addBackend(b);
        notifyBackendChange(b, BackendEvent.ADDED, null, null);
    }

    public void dispose() {
        final ILaunch[] launches = DebugPlugin.getDefault().getLaunchManager()
                .getLaunches();
        launchListener.launchesTerminated(launches);
        launchListener.dispose();
        epmdWatcherJob.stop();
    }

    public Map<Bundle, CodeBundleImpl> getCodeBundles() {
        return codeBundles;
    }
}

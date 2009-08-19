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
package org.erlide.runtime.backend.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.util.BackendUtils;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.preferences.OldErlangProjectProperties;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.BackendListener;
import org.erlide.jinterface.backend.BackendUtil;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.RuntimeVersion;
import org.erlide.jinterface.util.EpmdWatcher;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.jinterface.util.IEpmdListener;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.EpmdWatchJob;
import org.erlide.runtime.backend.ErlideBackend;
import org.erlide.runtime.backend.ErlideBackendVisitor;
import org.erlide.runtime.backend.ErtsProcess;
import org.erlide.runtime.launch.ErlLaunchAttributes;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpNodeStatus;

public final class BackendManagerImpl extends OtpNodeStatus implements
		IEpmdListener, BackendManager {

	private volatile ErlideBackend ideBackend;
	private final Object ideBackendLock = new Object();
	private final Map<IProject, Set<ErlideBackend>> executionBackends;
	private final Map<String, ErlideBackend> buildBackends;
	final List<BackendListener> listeners;
	private final List<CodeBundle> codeBundles;

	private final EpmdWatcher epmdWatcher;

	@SuppressWarnings("synthetic-access")
	private static final class LazyBackendManagerHolder {
		public static final BackendManager instance = new BackendManagerImpl();
	}

	public static final BackendManager getDefault() {
		return LazyBackendManagerHolder.instance;
	}

	private BackendManagerImpl() {
		ideBackend = null;
		executionBackends = new HashMap<IProject, Set<ErlideBackend>>();
		buildBackends = new HashMap<String, ErlideBackend>();
		listeners = new ArrayList<BackendListener>();
		codeBundles = new ArrayList<CodeBundle>();

		epmdWatcher = new EpmdWatcher();
		epmdWatcher.addEpmdListener(this);
		new EpmdWatchJob(epmdWatcher).schedule(100);
	}

	public ErlideBackend createBackend(final RuntimeInfo info,
			final Set<BackendOptions> options, final ILaunch launch)
			throws BackendException {
		final String nodeName = info.getNodeName();
		final boolean exists = EpmdWatcher.findRunningNode(nodeName);
		ErlideBackend b = null;

		final boolean isRemoteNode = nodeName.contains("@");
		if (exists || isRemoteNode) {
			ErlLogger.debug("create standalone " + options + " backend '"
					+ info + "' " + Thread.currentThread());
			b = new ErlideBackend(info);

		} else if (options.contains(BackendOptions.AUTOSTART)) {
			ErlLogger.debug("create managed " + options + " backend '" + info
					+ "' " + Thread.currentThread());
			b = new ErlideBackend(info);

			ManagedLauncher launcher = new ManagedLauncher(launch);
			launcher.startRuntime(info);
			IStreamsProxy streamsProxy = launcher.getStreamsProxy();
			b.setStreamsProxy(streamsProxy);
		}
		if (b == null) {
			ErlLogger.error("Node %s not found, could not launch!", nodeName);
			return null;
		}

		b.setLaunch(launch);
		if (launch != null) {
			DebugPlugin.getDefault().getLaunchManager().addLaunchListener(b);
		}
		initializeBackend(options, b);

		return b;
	}

	private void initializeBackend(final Set<BackendOptions> options,
			ErlideBackend b) {
		b.initializeRuntime();
		if (b.isDistributed()) {
			b.connect();
			for (CodeBundle bb : codeBundles) {
				b.register(bb.getBundle());
			}
			b.initErlang();
			b.registerStatusHandler(this);
			b.setDebug(options.contains(BackendOptions.DEBUG));
			b.setTrapExit(options.contains(BackendOptions.TRAP_EXIT));
		}
		notifyBackendChange(b, BackendEvent.ADDED);
	}

	private ErlideBackend createBackend(final RuntimeInfo info,
			final Set<BackendOptions> options) throws BackendException {
		ILaunchConfiguration launchConfig = getLaunchConfiguration(info,
				options);
		ILaunch launch;
		try {
			launch = launchConfig.launch(ILaunchManager.RUN_MODE,
					new NullProgressMonitor(), false, false);
		} catch (CoreException e) {
			e.printStackTrace();
			return null;
		}
		ErlideBackend b = createBackend(info, options, launch);
		return b;
	}

	public Backend getBuildBackend(final IProject project)
			throws BackendException {
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		final RuntimeInfo info = prefs.getRuntimeInfo();
		if (info == null) {
			ErlLogger.info("Project %s has no runtime info, using ide", project
					.getName());
			if (ideBackend == null) {
				throw new BackendException(
						"IDE backend is not created - check configuration!");
			}
			return ideBackend;
		}
		final String version = info.getVersion().asMajor().toString();
		ErlideBackend b = buildBackends.get(version);
		if (b == null) {
			info.setNodeName(version);
			info.setNodeNameSuffix("_" + BackendUtils.getErlideNameSuffix());
			info.setCookie("erlide");
			info.hasConsole(false);
			// will add workspace unique id
			b = createBackend(info, EnumSet.of(BackendOptions.AUTOSTART,
					BackendOptions.NO_CONSOLE, BackendOptions.INTERNAL));
			buildBackends.put(version, b);
		}
		ErlLogger.info("BUILD project %s on %s", project.getName(), info
				.getVersion());
		return b;
	}

	private ILaunchConfiguration getLaunchConfiguration(RuntimeInfo info,
			Set<BackendOptions> options) {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		ILaunchConfigurationType type = manager
				.getLaunchConfigurationType(ErtsProcess.CONFIGURATION_TYPE);
		ILaunchConfigurationWorkingCopy workingCopy;
		try {
			workingCopy = type.newInstance(null, "some erlang node");
			workingCopy.setAttribute(ErlLaunchAttributes.NODE_NAME, info
					.getNodeName());
			workingCopy.setAttribute(ErlLaunchAttributes.RUNTIME_NAME, info
					.getName());
			workingCopy.setAttribute(ErlLaunchAttributes.COOKIE, info
					.getCookie());
			if (options.contains(BackendOptions.NO_CONSOLE)) {
				workingCopy.setAttribute(ErlLaunchAttributes.CONSOLE, false);
			}
			if (options.contains(BackendOptions.INTERNAL)) {
				workingCopy.setAttribute(ErlLaunchAttributes.INTERNAL, true);
			}
			return workingCopy.doSave();
		} catch (CoreException e) {
			e.printStackTrace();
			return null;
		}
	}

	public synchronized Set<ErlideBackend> getExecutionBackends(
			final IProject project) {
		final Set<ErlideBackend> bs = executionBackends.get(project);
		if (bs == null) {
			return Collections.emptySet();
		}
		return Collections.unmodifiableSet(bs);
	}

	public ErlideBackend getIdeBackend() {
		// System.out.println("GET ide" + Thread.currentThread());
		if (ideBackend == null) {
			synchronized (ideBackendLock) {
				if (ideBackend == null) {
					try {
						System.out.println("CREATE ide");
						createIdeBackend();
					} catch (BackendException e) {
						ErlLogger.error("Could not start IDE backend: "
								+ e.getMessage());
					}
				}
			}
		}
		// System.out.println(">>> " + ideBackend);
		return ideBackend;
	}

	private void createIdeBackend() throws BackendException {
		final RuntimeInfo info = RuntimeInfo.copy(ErlangCore
				.getRuntimeInfoManager().getErlideRuntime(), false);
		if (info != null) {
			final String defLabel = BackendUtil.getLabelProperty();
			if (defLabel != null) {
				info.setNodeName(defLabel);
			} else {
				String nodeName = BackendUtils.getErlideNameSuffix()
						+ "_erlide";
				info.setNodeName(nodeName);
			}
			info.setCookie("erlide");
			info.hasConsole(false);
			ErlLogger.debug("creating IDE backend %s", info.getName());
			EnumSet<BackendOptions> options = EnumSet.of(
					BackendOptions.AUTOSTART, BackendOptions.INTERNAL);
			if (!ErlideUtil.isDeveloper()) {
				options.add(BackendOptions.NO_CONSOLE);
			}
			ideBackend = createBackend(info, options);
		} else {
			ErlLogger.error("There is no erlideRuntime defined! "
					+ "Could not start IDE backend.");
		}
	}

	public void addBackendListener(final BackendListener listener) {
		listeners.add(listener);
	}

	public void removeBackendListener(final BackendListener listener) {
		listeners.remove(listener);
	}

	public Collection<ErlideBackend> getAllBackends() {
		final Set<ErlideBackend> ebs = new HashSet<ErlideBackend>();
		ErlideBackend ide = getIdeBackend();
		if (ide != null) {
			ebs.add(ide);
		}
		for (final Set<ErlideBackend> b : executionBackends.values()) {
			ebs.addAll(b);
		}
		for (final ErlideBackend b : buildBackends.values()) {
			ebs.add(b);
		}
		return ebs;
	}

	public void addBundle(final Bundle b) {
		CodeBundle p = findBundle(b);
		if (p != null) {
			return;
		}
		p = new CodeBundle(b);
		codeBundles.add(p);
		forEachBackend(new ErlideBackendVisitor() {
			public void visit(final ErlideBackend bb) {
				bb.register(b);
			}
		});
	}

	public void removeBundle(final Bundle b) {
		final CodeBundle p = findBundle(b);
		if (p == null) {
			return;
		}
		codeBundles.remove(p);
		forEachBackend(new ErlideBackendVisitor() {
			public void visit(final ErlideBackend bb) {
				bb.unregister(b);
			}
		});
	}

	private CodeBundle findBundle(Bundle b) {
		for (CodeBundle p : codeBundles) {
			if (p.getBundle() == b) {
				return p;
			}
		}
		return null;
	}

	public void forEachBackend(final ErlideBackendVisitor visitor) {
		for (ErlideBackend b : getAllBackends()) {
			visitor.visit(b);
		}
	}

	public synchronized void updateNodeStatus(final String host,
			final Collection<String> started, final Collection<String> stopped) {
		for (final String b : started) {
			final String name = b + "@" + host;
			ErlLogger.info("(epmd) started: '%s'", name);
			remoteStatus(name, true, null);
		}
		for (final String b : stopped) {
			final String name = b + "@" + host;
			ErlLogger.info("(epmd) stopped: '%s'", name);
			remoteStatus(name, false, null);
		}

	}

	public boolean isCompatibleBackend(final IProject project,
			final ErlideBackend b) {
		RuntimeVersion projectVersion = ErlangCore
				.getProjectProperties(project).getRuntimeVersion();
		return b.getInfo().getVersion().isCompatible(projectVersion);
	}

	public synchronized void addExecutionBackend(final IProject project,
			final ErlideBackend b) {
		Set<ErlideBackend> list = executionBackends.get(project);
		if (list == null) {
			list = new HashSet<ErlideBackend>();
			executionBackends.put(project, list);
		}
		list.add(b);
	}

	public synchronized void removeExecutionBackend(final IProject project,
			final Backend b) {
		Set<ErlideBackend> list = executionBackends.get(project);
		if (list == null) {
			list = new HashSet<ErlideBackend>();
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
			for (final Entry<IProject, Set<ErlideBackend>> e : executionBackends
					.entrySet()) {
				for (final Backend be : e.getValue()) {
					final String bnode = be.getInfo().getNodeName();
					if (BackendUtil.buildNodeName(bnode, true).equals(node)) {
						removeExecutionBackend(e.getKey(), be);
						break;
					}
				}
			}
		}
	}

	public void dispose(final ErlideBackend backend) {
		if (backend != null && backend != ideBackend) {
			backend.dispose();
		}
	}

	@Override
	public void remoteStatus(final String node, final boolean up,
			final Object info) {
		// final String dir = up ? "up" : "down";
		// ErlLogger.debug(String.format("@@: %s %s %s", node, dir, info));

		for (final Backend bb : getAllBackends()) {
			if (bb != null) {
				if (node.equals(bb.getPeer())) {
					bb.setAvailable(up);
				}
			}
		}
		remoteNodeStatus(node, up, info);
	}

	@Override
	public void connAttempt(final String node, final boolean incoming,
			final Object info) {
		final String direction = incoming ? "in" : "out";
		ErlLogger.info(String.format("Connection attempt: %s %s: %s", node,
				direction, info));
	}

	void notifyBackendChange(final Backend b, final BackendEvent type) {
		if (listeners == null) {
			return;
		}

		final Object[] copiedListeners = listeners.toArray();
		for (final Object element : copiedListeners) {
			BackendListener listener = (BackendListener) element;
			switch (type) {
			case ADDED:
				listener.runtimeAdded(b);
				break;
			case REMOVED:
				listener.runtimeRemoved(b);
				break;
			}
		}
	}

}

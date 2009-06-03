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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.util.BackendUtils;
import org.erlide.core.preferences.OldErlangProjectProperties;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.BackendListener;
import org.erlide.jinterface.backend.BackendUtil;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.util.EpmdWatcher;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.jinterface.util.IEpmdListener;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.EpmdWatchJob;
import org.erlide.runtime.backend.ErlideBackend;
import org.erlide.runtime.backend.ErlideBackendVisitor;
import org.erlide.runtime.backend.ICodeBundle;

import com.ericsson.otp.erlang.OtpEpmd;
import com.ericsson.otp.erlang.OtpNodeStatus;

public final class BackendManagerImpl extends OtpNodeStatus implements
		IEpmdListener, BackendManager {

	private volatile ErlideBackend ideBackend;
	private final Object ideBackendLock = new Object();
	private final Map<IProject, Set<ErlideBackend>> executionBackends;
	private final Map<String, ErlideBackend> buildBackends;
	final List<BackendListener> listeners;
	private final List<ICodeBundle> plugins;

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
		plugins = new ArrayList<ICodeBundle>();

		epmdWatcher = new EpmdWatcher();
		epmdWatcher.addEpmdListener(this);
		new EpmdWatchJob(epmdWatcher).schedule(100);
	}

	public ErlideBackend create(final RuntimeInfo info,
			final Set<BackendOptions> options, final ILaunch launch)
			throws BackendException {

		final String nodeName = info.getNodeName();
		final boolean exists = findRunningNode(nodeName);
		ErlideBackend b = null;

		final boolean isRemoteNode = nodeName.contains("@");
		if (exists || isRemoteNode) {
			ErlLogger.debug("create standalone " + options + " backend '"
					+ info + "' " + Thread.currentThread());
			b = new ErlideBackend(info, RuntimeLauncherFactory
					.createStandaloneLauncher(launch));
		} else if (options.contains(BackendOptions.AUTOSTART)) {
			ErlLogger.debug("create managed " + options + " backend '" + info
					+ "' " + Thread.currentThread());
			b = new ErlideBackend(info, RuntimeLauncherFactory
					.createManagedLauncher(launch));
		}
		if (b == null) {
			ErlLogger.error("Node %s not found, could not launch!", nodeName);
			return null;
		}

		b.initializeRuntime();
		b.connect();
		b.register(plugins);
		b.initErlang();
		b.registerStatusHandler(this);
		b.setDebug(options.contains(BackendOptions.DEBUG));
		b.setTrapExit(options.contains(BackendOptions.TRAP_EXIT));

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
			// will add workspace unique id

			b = create(info, EnumSet.of(BackendOptions.AUTOSTART), null);
			buildBackends.put(version, b);
		}
		return b;
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
		if (ideBackend == null) {
			synchronized (ideBackendLock) {
				if (ideBackend == null) {

					final RuntimeInfo erlideRuntime = RuntimeInfo.copy(
							ErlangCore.getRuntimeInfoManager()
									.getErlideRuntime(), false);
					if (erlideRuntime != null) {
						try {
							final String defLabel = BackendUtil
									.getLabelProperty();
							if (defLabel != null) {
								erlideRuntime.setNodeName(defLabel);
							} else {
								erlideRuntime.setNodeName(BackendUtils
										.getErlideNameSuffix()
										+ "_erlide");
							}
							erlideRuntime.setCookie("erlide");
							ErlLogger.debug("creating IDE backend %s",
									erlideRuntime.getName());
							ideBackend = create(erlideRuntime, EnumSet
									.of(BackendOptions.AUTOSTART), null);
						} catch (final BackendException e) {
							// erlideRuntime can't be null here
						}
					} else {
						ErlLogger
								.error("There is no erlideRuntime defined! Could not start IDE backend.");
					}
				}
			}
		}
		return ideBackend;
	}

	public void addBackendListener(final BackendListener listener) {
		listeners.add(listener);
	}

	public void removeBackendListener(final BackendListener listener) {
		listeners.remove(listener);
	}

	public Backend[] getAllBackends() {
		final Set<ErlideBackend> ebs = new HashSet<ErlideBackend>();
		for (final Set<ErlideBackend> b : executionBackends.values()) {
			ebs.addAll(b);
		}
		for (final ErlideBackend b : buildBackends.values()) {
			ebs.add(b);
		}
		final Object[] eb = ebs.toArray();
		final Backend b = getIdeBackend();
		final int x = (b == null) ? 0 : 1;
		final Backend[] res = new Backend[eb.length + x];
		System.arraycopy(eb, 0, res, 0, eb.length);
		if (b != null) {
			res[eb.length] = b;
		}
		return res;
	}

	public void addPlugin(final ICodeBundle p) {
		if (plugins.indexOf(p) < 0) {
			plugins.add(p);
			if (ideBackend != null) {
				ideBackend.getCodeManager().register(p);
				ideBackend.checkCodePath();
			}
			forEachProjectBackend(new ErlideBackendVisitor() {
				public void visit(final ErlideBackend b) {
					b.getCodeManager().register(p);
					b.checkCodePath();
				}
			});
		}
	}

	public void removePlugin(final ICodeBundle p) {
		plugins.remove(p);
		if (ideBackend != null) {
			ideBackend.getCodeManager().unregister(p);
		}
		forEachProjectBackend(new ErlideBackendVisitor() {
			public void visit(final ErlideBackend b) {
				b.getCodeManager().unregister(p);
			}
		});
	}

	public void forEachProjectBackend(final ErlideBackendVisitor visitor) {
		// TODO which backends?
	}

	public synchronized void updateNodeStatus(final String host,
			final List<String> started, final List<String> stopped) {
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

	public static boolean findRunningNode(final String nodeName) {
		try {
			final String[] names = OtpEpmd.lookupNames();
			final List<String> labels = EpmdWatcher.clean(Arrays.asList(names));
			for (final String name : labels) {
				if (name.equals(nodeName)) {
					return true;
				}
			}
		} catch (final IOException e) {
		}
		return false;
	}

	public EpmdWatcher getEpmdWatcher() {
		return epmdWatcher;
	}

	public void remoteNodeStatus(final String node, final boolean up,
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

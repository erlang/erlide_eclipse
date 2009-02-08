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
package org.erlide.runtime.backend;

import java.io.IOException;
import java.net.InetAddress;
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
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.ILaunch;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.ErlangProjectProperties;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.EpmdWatcher;
import org.erlide.jinterface.IEpmdListener;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.internal.RuntimeLauncherFactory;

import com.ericsson.otp.erlang.OtpEpmd;
import com.ericsson.otp.erlang.OtpNodeStatus;

public final class BackendManager implements IEpmdListener {

	private volatile Backend fLocalBackend;
	private final Object fLocalBackendLock = new Object();
	private final Map<IProject, Backend> fBuildBackends;
	private final Object fBuildBackendsLock = new Object();
	private final Map<IProject, Set<Backend>> fBackends;
	protected List<BackendListener> fListeners;
	private final List<ICodeBundle> fPlugins;

	private EpmdWatcher epmdWatcher;

	public enum BackendEvent {
		ADDED, REMOVED
	};

	public enum BackendOptions {
		DEBUG, AUTOSTART, TRAP_EXIT
	};

	@SuppressWarnings("synthetic-access")
	private static final class LazyBackendManagerHolder {
		public static final BackendManager instance = new BackendManager();
	}

	public static final BackendManager getDefault() {
		return LazyBackendManagerHolder.instance;
	}

	private BackendManager() {
		fLocalBackend = null;
		fBuildBackends = new HashMap<IProject, Backend>();
		fBackends = new HashMap<IProject, Set<Backend>>();
		fListeners = new ArrayList<BackendListener>();
		fPlugins = new ArrayList<ICodeBundle>();

		epmdWatcher = new EpmdWatcher();
		epmdWatcher.addEpmdListener(this);
		new EpmdWatchJob(epmdWatcher).schedule(100);
	}

	public static String getJavaNodeName() {
		String fUniqueId = getTimeSuffix();
		return "jerlide_" + fUniqueId;
	}

	static String getErlideNameSuffix() {
		String fUniqueId;
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		final String location = root.getLocation().toPortableString();
		fUniqueId = Long.toHexString(location.hashCode() & 0xFFFFFFF);
		return fUniqueId;
	}

	private static String getTimeSuffix() {
		String fUniqueId;
		fUniqueId = Long.toHexString(System.currentTimeMillis() & 0xFFFFFFF);
		return fUniqueId;
	}

	public Backend create(final RuntimeInfo info,
			final Set<BackendOptions> options, ILaunch launch)
			throws BackendException {

		String nodeName = info.getNodeName();
		boolean exists = findRunningNode(nodeName);
		Backend b = null;

		boolean isRemoteNode = nodeName.contains("@");
		if (exists || isRemoteNode) {
			ErlLogger.debug("create standalone " + options + " backend '"
					+ info + "' " + Thread.currentThread());
			b = new Backend(info, RuntimeLauncherFactory
					.createStandaloneLauncher());
		} else if (options.contains(BackendOptions.AUTOSTART)) {
			ErlLogger.debug("create managed " + options + " backend '" + info
					+ "' " + Thread.currentThread());
			b = new Backend(info, RuntimeLauncherFactory
					.createManagedLauncher());
		}
		if (b == null) {
			ErlLogger.error("Node %s not found, could not launch!", nodeName);
			return null;
		}

		b.initializeRuntime(launch);
		b.connectAndRegister(fPlugins);
		b.initErlang();
		b.setDebug(options.contains(BackendOptions.DEBUG));
		b.setTrapExit(options.contains(BackendOptions.TRAP_EXIT));

		return b;
	}

	public Backend getBuildBackend(final IProject project)
			throws BackendException {
		synchronized (fBuildBackendsLock) {
			final RuntimeInfo info = getRuntimeInfo(project);
			if (info == null) {
				// ErlLogger.info("Project %s has no runtime info, using ide",
				// project.getName());
				if (fLocalBackend == null) {
					throw new BackendException(
							"IDE backend is not created - check configuration!");
				}
				return fLocalBackend;
			}
			if (info.getNodeName() == null || info.getNodeName().equals("")) {
				if (fLocalBackend == null) {
					throw new BackendException(
							"IDE backend is not created - check configuration!");
				}
				return fLocalBackend;
			}

			Backend b = fBuildBackends.get(project);
			if (b != null && !b.ping()) {
				fBuildBackends.remove(info.getName());
				fireUpdate(b, BackendEvent.REMOVED);
				b = null;
			}
			if (b == null) {
				if (info.getNodeName() == null) {
					info.setNodeName(project.getName());
				}
				try {
					b = create(info, EnumSet.of(BackendOptions.AUTOSTART), null);
				} catch (BackendException e) {
					// info can't be null here
				}
				fBuildBackends.put(project, b);
				fireUpdate(b, BackendEvent.ADDED);
			}
			return b;
		}
	}

	synchronized public Set<Backend> getExecutionBackends(final IProject project) {
		Set<Backend> bs = fBackends.get(project);
		if (bs == null) {
			return Collections.emptySet();
		}
		return Collections.unmodifiableSet(bs);
	}

	public static RuntimeInfo getRuntimeInfo(final IProject project) {
		if (project == null) {
			return null;
		}
		final ErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		return prefs.getRuntimeInfo();
	}

	public void remove(final IProject project) {
		/*
		 * final String name = getBackendName(project); // TODO if (not used
		 * anymore) fLocalBackends.remove(name);
		 * fProjectBackendMap.remove(project);
		 */
	}

	public Backend getIdeBackend() {
		if (fLocalBackend == null) {
			synchronized (fLocalBackendLock) {
				if (fLocalBackend == null) {
					final RuntimeInfo erlideRuntime = RuntimeInfo.copy(
							ErlangCore.getRuntimeInfoManager()
									.getErlideRuntime(), false);
					if (erlideRuntime != null) {
						try {
							String defLabel = getLabelProperty();
							if (defLabel != null) {
								erlideRuntime.setNodeName(defLabel);
							} else {
								erlideRuntime.setNodeName(getErlideNameSuffix()
										+ "_erlide");
							}
							erlideRuntime.setCookie("erlide");
							fLocalBackend = create(erlideRuntime, EnumSet
									.of(BackendOptions.AUTOSTART), null);
						} catch (BackendException e) {
							// erlideRuntime can't be null here
						}
					} else {

					}
				}
			}
		}
		return fLocalBackend;
	}

	public static String getLabelProperty() {
		return System.getProperty("erlide.label", null);
	}

	public void addBackendListener(final BackendListener listener) {
		fListeners.add(listener);
	}

	public void removeBackendListener(final BackendListener listener) {
		fListeners.remove(listener);
	}

	private void fireUpdate(final Backend b, final BackendEvent type) {
		new BackendChangeNotifier().notify(b, type);
	}

	/**
	 * Notifies a backend listener of additions or removals
	 */
	class BackendChangeNotifier implements ISafeRunnable {

		private BackendListener fListener;

		private BackendEvent fType;

		private Backend fChanged;

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.core.runtime.ISafeRunnable#handleException(java.lang.
		 * Throwable)
		 */
		public void handleException(final Throwable exception) {
			final IStatus status = new Status(IStatus.ERROR,
					ErlangPlugin.PLUGIN_ID, 1, "backend listener exception",
					exception);
			ErlangPlugin.log(status);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.core.runtime.ISafeRunnable#run()
		 */
		public void run() throws Exception {
			switch (fType) {
			case ADDED:
				fListener.backendAdded(fChanged);
				break;
			case REMOVED:
				fListener.backendRemoved(fChanged);
				break;
			}
		}

		/**
		 * Notifies the given listener of the adds/removes
		 */
		public void notify(final Backend b, final BackendEvent type) {
			if (fListeners == null) {
				return;
			}

			fChanged = b;
			fType = type;
			final Object[] copiedListeners = fListeners.toArray();
			for (final Object element : copiedListeners) {
				fListener = (BackendListener) element;
				SafeRunner.run(this);
			}
			fChanged = null;
			fListener = null;
		}
	}

	public Backend[] getAllBackends() {
		synchronized (fBuildBackendsLock) {
			final Object[] ob = fBuildBackends.values().toArray();
			Set<Backend> ebs = new HashSet<Backend>();
			for (Set<Backend> b : fBackends.values()) {
				ebs.addAll(b);
			}
			final Object[] eb = ebs.toArray();
			Backend b = getIdeBackend();
			int x = (b == null) ? 0 : 1;
			final Backend[] res = new Backend[ob.length + eb.length + x];
			System.arraycopy(ob, 0, res, 0, ob.length);
			System.arraycopy(eb, 0, res, ob.length, eb.length);
			if (b != null) {
				res[ob.length + eb.length] = b;
			}
			return res;
		}
	}

	public void register(final ICodeBundle p) {
		if (fPlugins.indexOf(p) < 0) {
			fPlugins.add(p);
			if (fLocalBackend != null) {
				fLocalBackend.getCodeManager().register(p);
				fLocalBackend.checkCodePath();
			}
			forEachProjectBackend(new BackendVisitor() {
				public void run(final Backend b) {
					b.getCodeManager().register(p);
					b.checkCodePath();
				}
			});
		}
	}

	public void removePlugin(final ICodeBundle p) {
		fPlugins.remove(p);
		if (fLocalBackend != null) {
			fLocalBackend.getCodeManager().unregister(p);
		}
		forEachProjectBackend(new BackendVisitor() {
			public void run(final Backend b) {
				b.getCodeManager().unregister(p);
			}
		});
	}

	public void forEachProjectBackend(final BackendVisitor visitor) {
		synchronized (fBuildBackendsLock) {
			for (final Backend b : fBuildBackends.values()) {
				try {
					visitor.run(b);
				} catch (final Exception e) {
				}
			}
		}
	}

	public static String buildNodeName(final String label) {
		if (label.indexOf('@') > 0) {
			// ignore unique here?
			return label;
		}
		final String host = getHost();
		return label + "@" + host;
	}

	public static String getHost() {
		String host;
		try {
			host = InetAddress.getLocalHost().getHostName();
			if (System.getProperty("erlide.host") != null) {
				final int dot = host.indexOf(".");
				if (dot != -1) {
					host = host.substring(0, dot);
				}
			}
		} catch (final IOException e) {
			host = "localhost";
			ErlLogger.error(e);
		}
		return host;
	}

	synchronized public void updateNodeStatus(String host,
			final List<String> started, final List<String> stopped) {
		for (final String b : started) {
			String name = b + "@" + host;
			ErlLogger.info("(epmd) started: '%s'", name);
			for (final Backend bb : getAllBackends()) {
				if (bb != null) {
					((OtpNodeStatus) bb).remoteStatus(name, true, null);
				}
			}
		}
		for (final String b : stopped) {
			String name = b + "@" + host;
			ErlLogger.info("(epmd) stopped: '%s'", name);
			for (final Backend bb : getAllBackends()) {
				if (bb != null) {
					((OtpNodeStatus) bb).remoteStatus(name, false, null);
				}
			}
		}

	}

	synchronized public void addExecution(IProject project, Backend b) {
		Set<Backend> list = fBackends.get(project);
		if (list == null) {
			list = new HashSet<Backend>();
			fBackends.put(project, list);
		}
		list.add(b);
	}

	synchronized public void removeExecution(IProject project, Backend b) {
		Set<Backend> list = fBackends.get(project);
		if (list == null) {
			list = new HashSet<Backend>();
			fBackends.put(project, list);
		}
		list.remove(b);
	}

	public static boolean findRunningNode(String nodeName) {
		try {
			final String[] names = OtpEpmd.lookupNames();
			final List<String> labels = EpmdWatcher.clean(Arrays.asList(names));
			for (String name : labels) {
				if (name.equals(nodeName)) {
					return true;
				}
			}
		} catch (IOException e) {
		}
		return false;
	}

	public EpmdWatcher getEpmdWatcher() {
		return epmdWatcher;
	}

	public void remoteNodeStatus(String node, boolean up, Object info) {
		if (!up) {
			for (Entry<IProject, Set<Backend>> e : fBackends.entrySet()) {
				for (Backend be : e.getValue()) {
					String bnode = be.getInfo().getNodeName();
					if (buildNodeName(bnode).equals(node)) {
						removeExecution(e.getKey(), be);
						break;
					}
				}
			}
		}
	}

}
